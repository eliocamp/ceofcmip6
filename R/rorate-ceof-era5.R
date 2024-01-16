

rotate_ceof_era5 <- function(ceof, toc, enso, o3wave_lats) {
  
  wave1_o3 <- toc %>% 
    .[lat %between% o3wave_lats] %>% 
    .[, .(toc = weighted.mean(toc, w = cos(lat*pi/180), na.rm = TRUE)), by = .(time, lon)] %>% 
    .[, metR::FitWave(toc, 1), by = .(time)] 
  
  rotations_o3 <- ceof$left %>% 
    .[cEOF == "cEOF1"] %>% 
    .[wave1_o3, on = "time"] %>% 
    .[, eliotesis::correlate_complex(hgt, amplitude), by = .(cEOF)]
  
  best_rotation_cEOF1 <- rotations_o3[part == "Real"][, .SD[which.max(correlation), .(angle)], by = .(cEOF)]
  
  rotations_cEOF2 <-  ceof$left %>% 
    .[cEOF == "cEOF2"] %>% 
    enso[., on = "time"] %>% 
    na.omit() %>% 
    .[, eliotesis::correlate_complex(hgt, oni), by = .(cEOF)]
  
  best_rotation_cEOF2 <- rotations_cEOF2 %>% 
    tidyfast::dt_pivot_wider(names_from = part, values_from = correlation) %>% 
    .[Imaginario > 0] %>% 
    .[, .SD[which.min(abs(Real)), .(angle)], by = .(cEOF)]
  
  
  rotations <- rbind(best_rotation_cEOF1, best_rotation_cEOF2)
  
  rotate_ceof <- function(x, rot) {
    x$left <- x$left[rot, on = 'cEOF'][, hgt := eliotesis::rotate(hgt, angle)][, angle := NULL]
    x$right <- x$right[rot, on = 'cEOF'][, hgt := eliotesis::rotate(hgt, angle)][, angle := NULL]
    
    x
  }
  
  
  rbind(
    data.table::data.table(ceof = list(ceof), 
                           rotation = "unrotated"),
    data.table::data.table(ceof = list(copy(ceof) %>% 
                                         rotate_ceof(rotations)), 
                           rotation = "rotated"))
}


check_rotation <- function(ceof_rotated) {
  # Check that rotated and unrotated eofs
  # predict the same fields (up to floating point errors)
  bad_cells <- ceof_rotated[, predict(ceof[[1]]), by = rotation] %>%
    .[, hgt := as.numeric(Re(hgt))] %>%
    data.table::dcast(time + lon + lat + lev ~ rotation, value.var = "hgt") %>%
    .[, dif := rotated - unrotated] %>%
    .[abs(rotated - unrotated)  > 1e-10]
  
  if (nrow(bad_cells) != 0) {
    stop("Rotated and unrotated eofs are not equivalent!")
  }
  
  ceof <- ceof_rotated[rotation == "rotated"]$ceof[[1]]
}

