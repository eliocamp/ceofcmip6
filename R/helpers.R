
organise_ceof_era5 <- function(ceof_era5) {
  ceof_era5 <- data.table::data.table(model = "ERA5",
                                      experiment = "reanalysis",
                                      eof = list(ceof_era5))
  
  ceof_era5$eof[[1]]$sdev <- ceof_era5[, eof[[1]]]$sdev[, `:=`(correlation = 1,  angle = 0)]
  ceof_era5$eof[[1]]$left <- ceof_era5$eof[[1]]$left[, ensemble := factor("1.1.1.1")]
  ceof_era5$eof[[1]]$left[, time := as.integer(year(time))]
  
  data.table::setcolorder(ceof_era5$eof[[1]]$left, c("time", "ensemble", "cEOF", "hgt"))  
  return(ceof_era5[])
}


last_modified <- function(files) {
  max(file.info(files)$mtime)
}


target_groups <- function(...) {
  as.numeric(interaction(..., drop = TRUE))
}


cmip_filter_complete_historical <- function(sims)  {
  sims %>% 
    dcast(source_id + experiment_id + member_id ~ variable_id, fun.aggregate = length) %>% 
    .[(tos != 0 & zg != 0) | experiment_id != "historical"] %>% 
    .[] %>% 
    .[, .(source_id, experiment_id, member_id)] %>% 
    sims[., on = .NATURAL]
}

cmip_remove_bad_sources <- function(sims) {
  # EC-Earth3 da muchos problemas. Los miembros no tienen todos el mismo
  # rango de fechas y eso ya me da errores más abajo... se va.
  
  # ICON-ESM-LR no tiene una grilla lon lat. Tiene una coordenada
  # i que identifica el punto de grilla.
  sims[!(source_id %in% c("EC-Earth3", "ICON-ESM-LR"))]
}

cmip_best_grid <- function(sims) {
  
  # Me quedo con la "mejor grilla". Esto afecta únicamente
  # a tos (sst).
  # El orden de preferencia de las grillas es
  # medio aleatorio; gn es el más común.
  grid_order <- c("gn", "gr", "gr1")
  
  sims %>% 
    data.table::copy() %>% 
    .[, grid_label := factor(grid_label, levels = grid_order, ordered = TRUE)] %>% 
    .[order(grid_label)] %>% 
    .[, .SD[grid_label == grid_label[1]], by = .(source_id, ensemble, variable_id, experiment_id)]
  
}



cmip_filter_zero_size <- function(sims) {
  nulos <- vapply(sims$files, function(x) any(file.size(x) == 0), FUN.VALUE = logical(1))
  
  if (any(nulos)) {
    warning("Archivo(s) nulos: \n", paste0(sims$id[nulos], collapse = "\n"))
  }
  
  return(sims[!nulos, ])
}

cmip_year_range <- function(files) {
  date_range_regex <- "(\\d{4})\\d{2}[-_](\\d{4})\\d{2}"
  dates <- utils::strcapture(date_range_regex, basename(files),
                             proto = list(file_date_start = integer(),
                                          file_date_end = integer()))
  list(min_year = min(dates$file_date_start), 
       max_year = max(dates$file_date_end))
}

same_range <- function(mins, maxs) {
  all(mins == mins[1] & maxs == maxs[1])
}


cmip_filter_incomplete_range <- function(sims) {
  sims[, c("min_year", "max_year") := cmip_year_range(files[[1]]), 
       by = .(source_id, ensemble, variable_id, experiment_id)] %>% 
    .[, same_range := same_range(min_year, max_year), by = .(source_id, variable_id, experiment_id)] %>% 
    .[same_range == TRUE] %>% 
    .[, `:=`(min_year = NULL, max_year = NULL, same_range = NULL)]
}



cmip_select_best_version <- function(sims) {
  select_most <- function(x) {
    x[, .N, by = version][which.max(N), version]
  }
  
  sims[, .SD[version == select_most(.SD)], by = .(id, member_id, experiment_id)]
}


cmip_select_required_variables <- function(sims, required_variables) {
  
  sims[, .SD[all(required_variables %in% variable_id)], by = .(id, member_id, experiment_id, version)]
  
}


