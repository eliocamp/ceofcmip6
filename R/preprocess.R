



sims <- data_path("derived", "sims.Rds") %>%
  readRDS()

dir <- data_path("derived", "cmip", "SH")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

future::plan("multicore", workers = 20)
Sys.setenv(HDF5_USE_FILE_LOCKING = FALSE)

files <- sims %>%
  .[variable_id == "zg"] %>%
  # .[source_id == "HadGEM3-GC31-LL" & experiment_id == "hist-nat"] %>%
  # .[ensemble == ensemble[1]] %>%
  .$files %>%
  furrr::future_map_chr(cmip_select_SH, folder = data_path("derived", "cmip", "SH"))

files_son <- sims$files %>%
  vapply(cmip_select_season, character(1), season = "SON", folder =  data_path("derived", "cmip", "SON"))

files <- c(files, files_son)

cuales_fallidos <- which(is.na(files))

if (length(cuales_fallidos) != 0) {

  fallidos <- sims %>%
    .[variable_id == "zg"] %>%
    .[cuales_fallidos] %>%
    .[, paste(source_id,  ensemble, experiment_id, collapse = "\n")]

  cli::cli_alert_danger(paste0("Fallaron: \n", fallidos))

} else {

  cli::cli_alert_success("Todos los archivos pre-procesados!")
}

httr::POST("ntfy.sh/elio-critical", body = "CMIP terminado!")
