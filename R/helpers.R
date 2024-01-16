
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
