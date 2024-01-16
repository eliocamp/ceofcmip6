compute_enso_cmip <- function(file) {
  metR::ReadNetCDF(file, vars = c(sst = "tos"), 
                   subset = list(lat = c(-5, 5),
                                 time = c("1979-09-01", "2014-12-31"),
                                 lon = metR::ConvertLongitude(c(-170, -120)))) %>% 
    .[!is.na(sst)] %>% 
    .[, time := data.table::year(time)] %>% 
    .[, .(oni = mean(sst)), by = .(time)] %>% 
    .[, oni := (oni - mean(oni))/sd(oni)] %>% 
    .[]
}
