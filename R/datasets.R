library(magrittr)

southern_hemisphere <- c(20, 0, -90, 360)

# Generic functions to handle downloading from some common sources
#' @export
#' @rdname new_dataset
simple_download <- function(url) {
  force(url)
  function(file){
    utils::download.file(url, destfile = file)
    file
  }
}

#' @export
#' @rdname new_dataset
download_cds <- function(request) {
  force(request)
  function(file) {
    keyring::keyring_unlock(keyring = "ecmwfr", password = Sys.getenv("KEYRINGPASS"))
    
    user <- Sys.getenv("CDSUSER", NA)
    key <- Sys.getenv("CDSKEY", NA)
    filename <- basename(file)
    
    if (is.na(user) | is.na(key)) {
      stop("Please set the environmental variables CDSUSER and CDSKEY")
    }
    
    request$target <- filename
    
    ecmwfr::wf_set_key(user = user,
                       key = key,
                       service = "cds")
    
    files <- ecmwfr::wf_request(request = request,
                                user = user,
                                time_out = 5*3600,
                                path = dirname(file))
    file
  }
}

#' @export
#' @rdname new_dataset
download_webapi <- function(request) {
  force(request)
  function(file) {
    keyring::keyring_unlock(keyring = "ecmwfr", password = Sys.getenv("KEYRINGPASS"))
    
    user <- Sys.getenv("WEBAPIUSER", NA)
    key <- Sys.getenv("WEBAPIKEY", NA)
    filename <- basename(file)
    
    if (is.na(user) | is.na(key)) {
      stop("Please set the environmental variables CDSUSER and CDSKEY")
    }
    
    request$target <- filename
    
    ecmwfr::wf_set_key(user = user,
                       key = key,
                       service = "webapi")
    
    files <- ecmwfr::wf_request(request = request,
                                user = user,
                                time_out = 5*3600,
                                path = dirname(file))
    file
  }
}



#' Data locations
#'
#' Functions that return the location of different datasets. They check that
#' relevant file exist and then return it with a class that has a neat print method.
#'
#'
#' @param type character indicating the type of data. The resulting base will be type_data
#' @param ... characters to be concatenated with [file.path()]
#'
#' @export
data_path <- function(type = c("raw", "derived"), ...) {
  here::here("data", paste0(type[1], "_data"), ...)
}


zenodo_record <- "https://zenodo.org/record/6612429/"
zenodo_from_file <- function(file) {
  paste0(zenodo_record, "/files/", basename(file), "?download=1")
}


datasets <- list()

#' @export
print.eliotesis_datasets <- function(x, ...) {
  cat("Available datasets: \n")
  for (name in names(x)) {
    cat("  * ", name, "\n")
  }
  
}

#' @export
datasets_list <- function() {
  x <- datasets
  class(x) <- "eliotesis_datasets"
  x
}

#' @export
datasets_download <- function(force_download = FALSE, from_source = FALSE) {
  lapply(datasets_list(), function(x) x(force_download = force_download, from_source = from_source))
}


#' Crea una nueva base de datos
#'
#' Devuelve una función que hace todo lo necesario para descargar los datos y
#' devuelve el archivo para leer.
#'
#' @param name el nombre de la base de datos (usada para mensajes)
#' @param file el archivo donde se va a guardar
#' @param source una función que toma un nombre de archivo y descarga los datos
#'
#' @returns
#' Una función que devuelve el nombre de archivo.
#' Chequea que los datos existan y si no existen los descarga de Zenodo, a menos que
#' `zenodo_url` sea `NULL` o se llame a la función con el argumento `from_source` `TRUE`.
#' En cuyo caso, descarga los datos de la fuente original con la función `source`.
#' La función advierte si el checksum no es igual al registrado.
#' @export
new_dataset <- function(name,
                        file,
                        source,
                        zenodo = FALSE) {
  if (is.character(source)) {
    source <- simple_download(source)
  }
  
  zeonodo_url <- NULL
  if (isTRUE(zenodo)) {
    zeonodo_url <- zenodo_from_file(file)
  }
  dataset <- list(
    name = name,
    file = file,
    source = source,
    zenodo_url = zeonodo_url
  )
  
  fun <- function(force_download = FALSE, from_source = FALSE) {
    will_download <- isTRUE(force_download) || !file.exists(dataset$file)
    
    if (will_download) {
      dir.create(dirname(dataset$file), showWarnings = FALSE, recursive = TRUE)
      from_source <- from_source ||is.null(dataset$zenodo_url)
      if (from_source) {
        message("Downloading dataset ", dataset$name, " from remote source.")
        dataset$source(dataset$file)
      } else {
        message("Downloading dataset ", dataset$name, " from zenodo repository.")
        utils::download.file(dataset$zenodo_url, dataset$file)
      }
    }
    
    md5_file <- paste0(dataset$file, ".md5")
    
    md5 <- digest::digest(file = dataset$file, algo = "md5")
    if (file.exists(md5_file)) {
      md5_previous <- readLines(md5_file)
      
      if (md5_previous != md5) {
        warning("File for dataset ", dataset$name, " has incorrect checksum.")
      }
    } else {
      message("Creating md5 file for dataset ", dataset$name, ".")
      writeLines(md5, md5_file)
      
    }
    
    dataset$file
  }
  
  datasets <<- append(datasets, setNames(list(fun), name))
  
  fun
  
}

last_year <- 2022
#' Datasets
#'
#' Chequea que los datos existan y si no existen los descarga de Zenodo, a menos que
#' `zenodo_url` sea `NULL` o se llame a la función con el argumento `from_source` `TRUE`.
#' En cuyo caso, descarga los datos de la fuente original con la función `source`.
#' La función advierte si el checksum no es igual al registrado.
#' @param from_source Lógico indicando si hay que descargar los datos desde la fuente
#' primaria en vez del repositorio de Zenodo
#'
#'
#' @export
#' @rdname datasets
#' @name datasets
ERA5_TOC <- new_dataset(
  name = "era5_total_column_ozone",
  file = data_path("raw", "era5.toc.mon.mean.nc"),
  source = download_cds(
    list(
      format = "netcdf",
      variable = "total_column_ozone",
      product_type = "monthly_averaged_reanalysis",
      year = as.character(1950:last_year),
      month = formatC(1:12, width = 2, flag = 0),
      time = "00:00",
      area = southern_hemisphere,
      grid = c("2.5", "2.5"),
      dataset_short_name = "reanalysis-era5-single-levels-monthly-means",
      target = "ozone.era5.nc"
    )
  )
)




#' @export
#' @rdname datasets
ERA5_geopotential <- new_dataset(
  name = "era5_geopotential",
  file = data_path("raw", "era5.z.mon.mean.nc"),
  source = download_cds(
    list(
      format = "netcdf",
      product_type = "monthly_averaged_reanalysis",
      variable = c("geopotential"),
      pressure_level = c('1', '2', '3',
                         '5', '7', '10',
                         '20', '30', '50',
                         '70', '100', '125',
                         '150', '175', '200',
                         '225', '250', '300',
                         '350', '400', '450',
                         '500', '550', '600',
                         '650', '700', '750',
                         '775', '800', '825',
                         '850', '875', '900',
                         '925', '950', '975',
                         '1000'),
      year = as.character(1940:last_year),
      month = formatC(1:12, width = 2, flag = 0),   # Need all months to compute PSA
      time = "00:00",
      grid = c("2.5", "2.5"),
      area = southern_hemisphere,
      dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means"
    )
  )
)

#' 
#' 
#' #' @export
#' #' @rdname datasets
#' ERA5_temperature <- new_dataset(
#'   name = "era5_air_temperature",
#'   file = data_path("raw", "era5.air.mon.mean.nc"),
#'   source = download_cds(
#'     list(
#'       format = "netcdf",
#'       product_type = "monthly_averaged_reanalysis",
#'       variable = c("temperature"),
#'       pressure_level = c("1", "2", "3", "5", "7", "10", "20", "30", "50",
#'                          "70", "100", "125", "150", "175", "200", "225",
#'                          "250", "300", "350", "400", "450", "500", "550",
#'                          "600", "650", "700", "750", "775", "800", "825",
#'                          "850", "875", "900", "925", "950", "975", "1000"),
#'       year = as.character(1959:last_year),
#'       month = formatC(1:12, width = 2, flag = 0),
#'       time = "00:00",
#'       grid = c("2.5", "2.5"),
#'       area = southern_hemisphere,
#'       dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means"
#'     )
#'   )
#' )



#' #' @export
#' #' @rdname datasets
#' ERA5_vorticity <- new_dataset(
#'   name = "era5_vorticity",
#'   file = data_path("raw", "era5.vor.mon.mean.nc"),
#'   source = download_cds(
#'     list(
#'       format = "netcdf",
#'       product_type = "monthly_averaged_reanalysis",
#'       variable = c("vorticity"),
#'       pressure_level = c("200"),
#'       year = as.character(1959:last_year),
#'       month = formatC(1:12, width = 2, flag = 0),
#'       time = "00:00",
#'       grid = c("2.5", "2.5"),
#'       dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means"
#'     )
#'   )
#' )



#' #' @export
#' #' @rdname datasets
#' ERA5_ozone <- new_dataset(
#'   name = "era5_ozone",
#'   file = data_path("raw", "era5.o3.mon.mean.nc"),
#'   source = download_cds(
#'     list(
#'       format = "netcdf",
#'       product_type = "monthly_averaged_reanalysis",
#'       variable = c("ozone_mass_mixing_ratio"),
#'       pressure_level = c("1", "2", "3", "5", "7", "10", "20", "30", "50",
#'                          "70", "100", "125", "150", "175", "200", "225",
#'                          "250", "300", "350", "400", "450", "500", "550",
#'                          "600", "650", "700", "750", "775", "800", "825",
#'                          "850", "875", "900", "925", "950", "975", "1000"),
#'       year = as.character(1959:last_year),
#'       month = formatC(1:12, width = 2, flag = 0),
#'       time = "00:00",
#'       grid = c("2.5", "2.5"),
#'       area = southern_hemisphere,
#'       dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means"
#'     )
#'   )
#' )

#' #' @export
#' #' @rdname datasets
#' ERA5_T2M <- new_dataset(
#'   name = "era5_2_metre_temperature",
#'   file = data_path("raw", "era5.2mt.mon.mean.nc"),
#'   source = download_cds(
#'     list(
#'       format = "netcdf",
#'       variable = c("2m_temperature"),
#'       product_type = "monthly_averaged_reanalysis",
#'       year = as.character(1959:last_year),
#'       month = formatC(1:12, width = 2, flag = "0"),
#'       time = "00:00",
#'       grid = c("2.5", "2.5"),
#'       area = southern_hemisphere,
#'       dataset_short_name = "reanalysis-era5-single-levels-monthly-means"
#'     )
#'   )
#' )



# #' @export
#' # #' @rdname datasets
#' # NCEP_VOR <- data_nc_fun(file =  data_path("raw", "vor.mon.mean.nc"),
#' #                         download = "ftp://ftp2.psl.noaa.gov/Datasets/ncep.reanalysis.derived/sigma/vor.mon.mean.nc")
#' 
#' #' @export
#' #' @rdname datasets
#' CMAP <- new_dataset(
#'   name = "cmap_precipitation",
#'   file = data_path("raw", "cmap.mon.mean.csv"),
#'   source = function(file) {
#'     tempfile <- tempfile(fileext = ".nc")
#'     utils::download.file("ftp://ftp.cdc.noaa.gov/Datasets/cmap/std/precip.mon.mean.nc", tempfile)
#'     
#'     data <- metR::ReadNetCDF(tempfile, vars = c(pp = "precip"),
#'                              subset = list(time = c("1959-01-01", "2022-12-01")))
#'     data.table::fwrite(data, file)
#'     file
#'   }
#' )



#' @export
#' @rdname datasets
ERSST <- new_dataset(
  name = "ERSST",
  file = data_path("raw", "ersst.csv"),
  source = function(file) {
    base_url <- "https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/ersst.v5."
    
    years <- 1959:last_year
    months <- formatC(1:12, width = 2, flag = "0")
    dates <- data.table::CJ(years, months)[, paste0(years, months)]
    
    urls <- paste0(base_url, dates, ".nc")
    files <- data_path("raw", "ersst", paste0(dates, ".nc"))
    dir.create(dirname(files[1]), showWarnings = FALSE, recursive = TRUE)
    sink <- vapply(seq_along(urls), function(u) {
      
      if (file.exists(files[u])) {
        return(files[u])
      } 
      
      utils::download.file(urls[u], files[u])
      files[u]
    }, character(1))
    
    data <- lapply(files, function(file) metR::ReadNetCDF(file, vars = c(t = "sst")))
    data <- data.table::rbindlist(data)
    
    data.table::fwrite(data, file)
    file
  }
)

#' @export
#' @rdname datasets
ENSO <- new_dataset(
  name  = "ENSO_oceanic_enso_index",
  file = data_path("raw", "oni.csv"),
  source = function(file) {
    data <- rsoi::download_oni(use_cache = FALSE)
    
    data <- base::subset(data, Date >= as.Date("1959-01-01") & Date <= as.Date("2022-12-31"))
    
    data.table::fwrite(
      data.frame(
        time = lubridate::as_datetime(data$Date),
        oni = data$ONI
      ),
      file = file
    )
    file
  }
)

#' #' @export
#' #' @rdname datasets
#' DMI <- new_dataset(
#'   name  = "DMI",
#'   file = data_path("raw", "dmi.csv"),
#'   source = function(file) {
#'     data <- rsoi::download_dmi(use_cache = FALSE)
#'     
#'     data <- base::subset(data, Date >= as.Date("1959-01-01") & Date <= as.Date("2022-12-31"))
#'     data.table::fwrite(
#'       data.frame(
#'         time = lubridate::as_datetime(data$Date),
#'         dmi = data$DMI
#'       ),
#'       file = file
#'     )
#'     
#'     file
#'   }
#' )



#' #' @export
#' #' @rdname datasets
#' PSA <- new_dataset(
#'   name = "PSA",
#'   file = data_path("derived", "psa.Rds"),
#'   source = function(file) {
#'     psa <- ERA5_geopotential() %>%
#'       metR::ReadNetCDF(vars = c(hgt = "z"),
#'                        subset = list(level = list(500),
#'                                      latitude = -90:0,
#'                                      time = c("1979-01-01", "2022-12-01"))) %>%
#'       normalise_coords() %>%
#'       na.omit() %>%
#'       .[metR::is.full_season(time)] %>%
#'       .[, w := season_weights(time)] %>%
#'       .[, .(hgt = mean(hgt*w)), by = .(lon, lat, time = metR::seasonally(time))] %>%
#'       .[, value := metR::Anomaly(hgt)*sqrt(cos(lat*pi/180)),
#'         by = .(lon, lat, metR::season(time))] %>%
#'       EOF(value ~ time | lon + lat, n = 1:3, data = .)
#'     
#'     psa <- lapply(psa, function(x) {
#'       copy(x)[, PC := fcase(PC == "PC1", "SAM",
#'                             PC == "PC2", "PSA1",
#'                             PC == "PC3", "PSA2")]
#'     })
#'     
#'     saveRDS(psa, file)
#'     file
#'   }
#' )


#' @export
#' @rdname datasets
SAM <- new_dataset(
  name = "sam",
  file = data_path("derived", "sam.csv"),
  source = function(file) {
    
    levels <- metR::GlanceNetCDF(ERA5_geopotential())$dims$level$vals
    message("Computando SAMs...")
    sams <- lapply(setNames(levels, levels), function(l) {
      message("   ", l, "hPa")
      ERA5_geopotential() %>%
        metR::ReadNetCDF(vars = c("hgt" = "z"),
                         subset = list(latitude = c(-90, -20),
                                       level = l)) %>%
        eliotesis::normalise_coords() %>%
        .[, hgt := hgt/9.8] %>%
        na.omit() %>%
        .[, time := as.Date(time)] %>%
        .[, period := data.table::between(data.table::year(time), 1979, 2019)] %>%
        .[, hgt := metR::Anomaly(hgt, baseline = period), by = .(lon, lat, lev, data.table::month(time))] %>%
        .[, eliotesis::eof_asym(hgt, lon, lat, time, period = period), by = lev]
    })
    
    # Asegurar consistencia
    for (l in seq_along(sams)[-1]) {
      cor <- cor(sams[[l]][term == "full"]$estimate,
                 sams[[l-1]][term == "full"]$estimate)
      
      sams[[l]][, estimate := estimate*sign(cor)]
    }
    
    # Asegurar que el signo es consistente con la literatura
    aao <- rsoi::download_aao(TRUE, data_path("raw", "aao.csv")) %>%
      data.table::as.data.table() %>%
      .[, .(time = as.Date(Date), aao = AAO)] %>%
      .[ data.table::between(time, range(sams[[1]]$time)[1], range(sams[[1]]$time)[2])]
    
    cor <- sams[["700"]][term == "full"][aao, on = "time"][, cor(estimate, aao)]
    
    sams <- data.table::rbindlist(sams)
    
    sams[, estimate := estimate*sign(cor)]
    sams[, estimate_norm := estimate/stats::sd(estimate[term == "full"]), by = .(lev, PC)]
    
    data.table::fwrite(sams, file)
    file
  }
)

