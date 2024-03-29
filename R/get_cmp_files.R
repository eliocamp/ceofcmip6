cmip_files <- function() {
  
  cmip_root <- "/shera/datos/CMIP/"
  # Tuve que optimizar algunas cosas para poder parsear miles de archivos
  
  cli::cli_alert_info("Buscando archivos disponibles...")
  sims <- eliotesis::cmip_available(mip_era = "CMIP6",
                         activity_drs = "CMIP",
                         experiment_id = "historical",
                         # source_id = unique(simulaciones$model),
                         table_id = c("Amon", "Omon"),
                         variable_id = c("zg", "tos"), 
                         root = cmip_root) |> 
    data.table::as.data.table() |> 
    _[, version := as.numeric(version)] |> 
    _[, c("ensemble", "init", "physics", "forcing") := as.list(unglue::unglue_data(member_id, "r{ensemble}i{init}p{physics}f{forcing}", convert = TRUE))] |> 
    _[order(ensemble)]
  
  # Algunos modelos tienen distintas parametrizaciones, inicializaciones y demás.
  # Me quedo sólo con una combinación para no volverme loque.
  
  ## TODO: asegurarse de que coincidan los tos y zg.
  # sims <- sims |>
  #   # _[source_id == "MRI-ESM2-0" & variable_id == "zg"] |>
  #   _[, n := .N, by =  .(source_id, variable_id, physics,  init, forcing)] |>
  #   _[, .SD[n == max(n)], by = .(source_id, variable_id)] |>
  #   _[, n]
  
  sims <- sims[, .SD[physics == physics[1] & init == init[1] & forcing == forcing[1]],
               by =  .(source_id, variable_id)] |>
    _[, .SD[data.table::uniqueN(ensemble) >= 5], by = source_id]
  
  # EC-Earth3 da muchos problemas. Los miembros no tienen todos el mismo
  # rango de fechas y eso ya me da errores más abajo... se va.
  sims <- sims[!(source_id == "EC-Earth3")]
  
  # ICON-ESM-LR no tiene una grilla lon lat. Tiene una coordenada
  # i que identifica el punto de grilla.
  sims <- sims[source_id != "ICON-ESM-LR"]
  
  # Me quedo con la "mejor grilla". Esto afecta únicamente
  # a tos (sst).
  # El orden de preferencia de las grillas es
  # medio aleatorio; gn es el más común.
  grid_order <- c("gn", "gr", "gr1")
  
  sims <- sims |>
    data.table::copy() |>
    _[, grid_label := factor(grid_label, levels = grid_order, ordered = TRUE)] |>
    _[order(grid_label)] |>
    _[, .SD[grid_label == grid_label[1]], by = .(source_id, ensemble, variable_id)]
  
  
  damip_experiments <- c("hist-GHG", "hist-stratO3", "hist-nat", "hist-aer")
  sims_damip <- eliotesis::cmip_available(mip_era = "CMIP6",
                               activity_drs = "DAMIP",
                               experiment_id = damip_experiments,
                               # source_id = unique(simulaciones$model),
                               table_id = c("Amon"),
                               variable_id = c("zg"),
                               root = cmip_root) |>
    data.table::as.data.table() |>
    _[, version := as.numeric(version)] |>
    _[, c("ensemble", "init", "physics", "forcing") := as.list(unglue::unglue_data(member_id, "r{ensemble}i{init}p{physics}f{forcing}", convert = TRUE))] |>
    _[, ensemble := interaction(ensemble, init, physics, forcing)]
  
  
  sims <- rbind(sims, sims_damip)
  
  sims[, id := paste(mip_era, activity_drs, institution_id, source_id, sep = ".")]

  
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
