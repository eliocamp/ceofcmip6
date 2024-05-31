
sh_dir <- data_path("derived", "cmip", "SH")
dir.create(sh_dir, recursive = TRUE, showWarnings = FALSE)

son_dir <- data_path("derived", "cmip", "SON")
dir.create(son_dir, recursive = TRUE, showWarnings = FALSE)

Sys.setenv(HDF5_USE_FILE_LOCKING = FALSE)

main_period <- c("1979-01-01", "2020-12-01")
o3wave_lats <- c(-75, -45)

times <- seq(as.Date("1979-01-01"), as.Date("2014-11-01"), by = "1 month")
times <- range(metR::seasonally(times[metR::season(times) == "SON"]))
