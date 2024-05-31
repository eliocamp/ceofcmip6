
Sys.setenv(HDF5_USE_FILE_LOCKING = FALSE)

cmip_apply_plan <- function(files_in) {
  plan <- attr(files_in, "plan")
  
  file_out <- files_in
  
  for (p in plan) {
    old_files <- file_out
    
    file_out <- p(file_out)
    
    cleanup_temps(old_files)
  }
   
  if (!file.exists(file_out) || length(file_out) == 0 || is.null(file_out)) {
    stop("Failed too apply plan ", files_in[[1]])
  }
  
  if (length(file_out) > 1) {
    stop("Plan ", files_in[[1]], " returned more than 1 file.")
  }
  
  return(file_out)
}


cleanup_temps <- function(files) {
  for (file in files){
    if (dirname(file) == tempdir()) {
      unlink(file)
    }  
  }
}

cmip_select_son_plan <- function(files_in, folder) {
  file_out <- file.path(folder, basename(files_in[1]))
  plan <- list()
  
  unique_items <- files_in %>%
    basename() %>%
    unglue::unglue_data("{variable}_{mean}_{model}_{experiment}_r{member}i{init}p{physics}f{forcing}_{grid}_{start_date}-{end_date}.nc") %>%
    data.table::as.data.table() %>%
    .[, ":="(start_date = NULL, end_date = NULL)] %>%
    unique()
  
  stopifnot(nrow(unique_items) == 1)
  
  var <- substr(basename(files_in[1]), 1, 1)
  if (var == "t") {
    if (startsWith(unique_items$model, "IPSL")) {
      plan <- c(plan, tos_IPSL = cmip_regrid_IPSL)
      
    } else {
      plan <- c(plan, tos_regrid = cmip_regrid)
    }
  }
  
  plan <- c(plan, 
            cmip_select = cmip_select_plan(files_in), 
            cmip_mergetime = cmip_mergetime_plan(file_out))
  
  attr(files_in, "plan") <- plan
  files_in
  
}

# IPSL tiene una grilla rara en tos
cmip_regrid_IPSL <- function(files_in) {
  vapply(files_in, cmip_fix_tos_grid, character(1))
}


cmip_fix_tos_grid <- function(file) {
  only_tos <- tempfile(basename(file))
  error <- TRUE
  on.exit(if (error) unlink(only_tos))
  # Only select tos
  res <- system_run(paste("cdo select,name='tos'", shQuote(file), shQuote(only_tos)))
  
  if (system_failed(res)) {
    stop("Failed to select tos in file ", basename(file))
  }
  
  metadata <- metR::GlanceNetCDF(only_tos)
  lat_var <- names(metadata$vars)[grep("lat", names(metadata$vars))]
  lat_var <- lat_var[!grepl("bounds|bnds", lat_var)]
  
  lon_var <- names(metadata$vars)[grep("lon", names(metadata$vars))]
  lon_var <- lon_var[!grepl("bounds|bnds", lon_var)]
  
  if (length(lon_var) > 1 || length(lat_var)> 1) {
    stop("Failed to extract only one lonlat in file ", basename(file))
  }
  if (length(lon_var) != 0) {
    res <- system_run(paste0("ncatted -a coordinates,tos,c,c,'",  paste(lon_var, lat_var), "' ", only_tos))
  }
  if (!is.null(attr(res, "status"))) {
    stop("Failed to change coordinates in file ", basename(file))
  }
  
  error <- FALSE
  return(only_tos)
}

# Algunos modelos  vienen con una grilla "genérica" en vez de lonlat
cmip_regrid <- function(files_in) {
  vapply(files_in, cmip_regrid_one, FUN.VALUE = character(1))
}


cmip_regrid_one <- function(file) {
  
  grid <- tempfile(basename(file))
  on.exit(unlink(grid))
  
  invisible(capture.output(t <- system(paste0("cdo griddes ", file, " > ", grid), intern = TRUE)))
  
  if (length(grep("generic", readLines(grid))) != 0) {
    
    invisible(capture.output(t <- system(paste0('sed -i "s/generic/lonlat/g" ', grid), intern = TRUE)))
    
    regridded_file <- tempfile(basename(file))
    invisible(capture.output(t <- system(paste0("cdo setgrid,", grid, " ", file, " ", regridded_file))))
    file <- regridded_file
    
  }  
  
  return(file)
  
}

cmip_select_plan <- function(files_in) {
  var <- substr(basename(files_in[1]), 1, 1)
  if (var == "z") {
    select <- "-sellevel,20000,5000"
    remap <- NULL
  } else {
    select <- NULL
    # Paso sst a una grilla regular porque sino distintos modelos tienen distintas
    # grillas y es un quilombo procesarlos programáticamente.
    remap <- "-remapdis,r240x60"
  }
  
  
  function(files_in) {
    
    files_out <- vapply(basename(files_in), tempfile, FUN.VALUE = character(1))
      
    error <- TRUE
    on.exit(if (error) unlink(files_out))
    
    for (f in seq_along(files_in)) {
      command <- paste0("cdo ", select, " -timselmean,3 -select,season=SON", " ",
                        remap, " ",
                        shQuote(files_in[f]), " ",
                        shQuote(files_out[f]))
      message("... corriendo: \n      ", command)
      
      s <- system_run(command)
      
      if (!file.exists(files_out[f])) {
        stop("Error in cmip_select in file ", files_in[1])
      }
    }
    
    error <- FALSE
    return(files_out)
  }
}


cmip_mergetime_plan <- function(file_out) {
  force(file_out)
  function(files_in) {
    
    unlink(file_out)
    
    s <- system_run(paste0("cdo mergetime ", paste0(shQuote(files_in), collapse = " "), " ",
                           shQuote(file_out)))
    if (system_failed(s)) {
      stop("Fallo al seleccionar el archivo ", files_in[1])
    }
    return(file_out)  
  } 
}



system_run <- function(command) {
  invisible(msg <- capture.output(t <- system(command, intern = TRUE)))  
  return(t)
}

system_failed <- function(status) {
  !is.null(attr(status, "status"))
}
