cmip_available <- function (..., root = cmip_root) {
  
  template_folder <- rcmip6:::cmip6_folder_template %>%
    gsub("\\%\\(", "{", .) %>% 
    gsub("\\)s", "}", .)
  
  template_file <- rcmip6:::cmip6_file_template[1] %>% 
    gsub("\\%\\(", "{", .) %>% 
    gsub("\\)s", "}", .)
  
  template <- file.path(template_folder, template_file)
  
  vars_folder <- template_folder %>%
    unglue::unglue(x = ., patterns = .) %>% 
    .[[1]] %>% 
    names()
  
  
  vars_file <- template_file %>%
    unglue::unglue(x = ., patterns = .) %>% 
    .[[1]] %>% 
    names()
  
  vars <- union(vars_file, vars_folder)
  
  search_null <- rep("*", length(vars)) %>% setNames(vars) %>% 
    as.list()
  
  globulate <- function(x) {
    if (length(x) > 1) {
      paste0("@(", paste0(unique(x), collapse = "|"), 
             ")")
    }
    else {
      x
    }
  }
  search <- list(...)
  
  for (name in names(search)) {
    search_null[[name]] <- search[[name]]
  }
  
  search <- search_null
  search <- lapply(search, globulate)
  search$root <- root
  
  file <- tempfile()
  file.create(file)
  
  command <- paste0("shopt -s extglob\n for f in ", paste0(glue::glue_data(search, template)), '; do echo "$f" >> ', file, '; done')
  
  script_file <- tempfile()
  writeLines(command, script_file)
  info <- system(paste0("/bin/bash  ", script_file), intern = TRUE)
  info <- readLines(file)
  
  info <- normalizePath(info)

  data <- unglue::unglue_data(basename(info), template_file)
  data_folder <- unglue::unglue_data(gsub(root, "", dirname(info)), 
                              gsub("\\{root\\}/", "", template_folder))
  
  cols <- setdiff(colnames(data_folder), colnames(data))
  
  data[cols] <- data_folder[cols]
    
  data$file <- info
  
  data.table::setDT(data) 
  
  data <- data[, .(files = list(file)), by = setdiff(colnames(data), c("file", "time_start", "time_end"))]
  
  return(data)
}


cmip_files <- function() {
  
  cmip_root <- "/shera/datos/CMIP/"
  # Tuve que optimizar algunas cosas para poder parsear miles de archivos
  
  cli::cli_alert_info("Buscando archivos disponibles...")
  
  sims <- cmip_available(mip_era = "CMIP6",
                         activity_drs = c("CMIP", "DAMIP"),
                         experiment_id = c("historical", "hist-GHG", "hist-stratO3", "hist-nat", "hist-aer"),
                         # source_id = unique(simulaciones$model),
                         table_id = c("Amon", "Omon"),
                         variable_id = c("zg", "tos", "ua"),
                         root = cmip_root) %>%
    .[, c("ensemble", "init", "physics", "forcing") := as.list(unglue::unglue_data(member_id, "r{ensemble}i{init}p{physics}f{forcing}", convert = TRUE))] %>%
    .[order(ensemble)] %>% 
    .[, id := paste(mip_era, activity_drs, institution_id, source_id, sep = ".")]
  
  
  
  sims[]
}


save_cmip6_citations <- function(sims) {
  urls <- paste0("https://www.wdc-climate.de/ui/cerarest/cmip6?input=", sims$id, "&exporttype=bibtex")
  
  get_citation_file <- function(url) {
    temp_file <- tempfile()
    res <- httr::GET(url = url, httr::accept("application/*"), httr::write_disk(temp_file, overwrite = TRUE))
    
    if (res$status_code == 200) {
      temp_file
    } else {
      NA_character_
    }
  }
  
  files <- vapply(unique(urls), get_citation_file, character(1))
  
  # files <- files[-27]
  all_lines <- vector("character")
  for (i in seq_along(files)) {
    lines <- readLines(files[i])
    lines[1] <- paste0("@article{", unique(sims$id)[i], ",")
    all_lines <- c(all_lines, lines, "\n")
  }
  
  
  cmip_bib_file <- here::here("paper/bib/cmip-models.bib")
  dir.create(dirname(cmip_bib_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(all_lines, cmip_bib_file)
  
  cmip_bib_file
}
