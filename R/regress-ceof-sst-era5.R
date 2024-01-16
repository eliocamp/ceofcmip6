rotate_reim <- function(real, imaginary, angle = 0) {
  z <- complex(real = real, imaginary = imaginary)
  z <- eliotesis::rotate(z, angle)
  list(real = Re(z),
       imaginary = Im(z))
}

regress_ceof_sst_era5 <- function(ceof, sst) {
  
  angles_regr <- -c(0, 45, 90, 45+90)*pi/180
  
  sst_for_regression <- ceof$left %>%
    # .[cEOF == "cEOF2"] %>% 
    data.table::copy() %>% 
    eliotesis::sep_ReIm(format = "wide") %>%
    .[, hgt := NULL] %>%
    .[sst, on = "time", allow.cartesian = TRUE] %>%
    data.table::setnames(c("Real", "Imaginario"), c("Real_bk", "Imaginario_bk")) %>% 
    na.omit() 
  
  sst_regr <- lapply(angles_regr, function(a) {
    sst_for_regression %>% 
      .[, c("Real", "Imaginario") := rotate_reim(Real_bk, Imaginario_bk, a)] %>% 
      .[, `:=`(Real = Real/sd(Real), Imaginario = Imaginario/sd(Imaginario)), 
        by = .(cEOF)] %>%
      .[, metR::FitLm(t, Imaginario, Real, se = TRUE), by = .(lon, lat, cEOF)] %>%
      eliotesis::rm_intercept() %>%
      .[term == "Real"] %>% 
      .[, term := eliotesis::factor_ReIm(term)] %>% 
      .[, angle := a*180/pi]
  }) %>% 
    data.table::rbindlist()
}
