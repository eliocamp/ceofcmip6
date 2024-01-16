
interpolate_to_era <- function(data, grid, formula = hgt ~ lon + lat) {
  metR::Interpolate(formula, x.out = unique(grid$lon), y.out = unique(grid$lat), 
              data = eliotesis::pad_longitudes(data)) 
}
