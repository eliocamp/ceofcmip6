library(data.table)
read_sst <- function(sst_files, time = c("1979-10-01", "2014-12-31"), lat = c(-90, 10)) {
  
  data <- lapply(seq_len(nrow(sst_files)), function(i) {
    metR::ReadNetCDF(sst_files[i, ]$file, c(sst = "tos"), 
               subset = list(lat  = lat, 
                             time = time)) %>% 
      .[!is.na(sst)] %>% 
      .[, time := year(time)] %>% 
      .[, ensemble := sst_files[i, ]$member] 
  })
  
  data <- data.table::rbindlist(data) %>% 
    .[, sst := sst - mean(sst), by = .(lon, lat, ensemble)]
  data
}

regress_sst <- function(simulaciones, ceofs,
                        time = c("1979-10-01", "2014-12-31"), 
                        lat = c(-90, 10)) {
  
  sst_files <- simulaciones %>% 
    .[variable == "tos"] 
  
  if (nrow(sst_files) == 0) {
    return(NULL)
  }
  
  sst_data <- read_sst(sst_files, time = time, lat = lat)
  
  standardised <- ceofs[, eof[[1]]$left, by = .(experiment, model)] %>% 
    .[, hgt := hgt/sd(Mod(hgt)), by = .(model, ensemble, cEOF)]
  
  regression <- standardised %>% 
    .[ensemble %in% unique(sst_files$member)] %>% 
    sst_data[., on = c("time", "ensemble"), allow.cartesian = TRUE] %>% 
    sep_ReIm(format = "wider") %>% 
    .[, FitLm(sst, Real, Imaginario, se = TRUE), 
      by = .(lon, lat, cEOF, model, experiment)] %>% 
    .[term != "(Intercept)"]
  
  return(regression)
}

