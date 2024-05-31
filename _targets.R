# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("eliotesis",
               "metR",
               "data.table",
               "magrittr") # packages that your targets need to run
  , format = "qs" # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  , controller = crew::crew_controller_local(workers = 10)
  , error = "abridge"
  , debug = "ceof_cmip_c638c84b" # hars dims....
  #
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package. The following
  # example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     workers = 50,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.0".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(NULL
     # Descargar todas los datasets
     , tar_target(era5, ERA5_geopotential() %>%
                    ReadNetCDF(vars = c(hgt = "z"),
                               subset = list(level = list(200, 50),
                                             latitude = -90:10,
                                             time = main_period)) %>%
                    .[, hgt := hgt/9.8] %>%
                    normalise_coords() %>%
                    na.omit() %>%   # Tengo que omitirlo por la forrada del expver
                    .[season(time) == "SON"] %>%
                    # .[is.full_season(time)] %>%
                    .[, w := season_weights(time)] %>%
                    .[, .(hgt = mean(hgt*w)), by = .(lev, lon, lat, time = seasonally(time))])
     
     , tar_target(enso, ENSO() %>%
                    fread() %>% 
                    na.omit() %>%
                    .[is.full_season(time)] %>% 
                    .[, oni := oni*season_weights(time)] %>% 
                    .[, .(oni = mean(oni)), by = .(time = seasonally(time))] %>% 
                    .[, `:=`(model = "ERA5", member = factor("1.1.1.1"), experiment = "reanalysis")])
     
     , tar_target(sam, SAM())
     
     , tar_target(sst, ERSST() %>% 
                    fread() %>% 
                    .[time %between% as.Date(main_period)] %>% 
                    .[is.full_season(time)] %>% 
                    .[season(time) == "SON"] %>% 
                    normalise_coords() %>%
                    .[, w := season_weights(time)] %>% 
                    .[, .(t = mean(t*w)), by = .(lon, lat, time = seasonally(time))])
     
     , tar_target(toc, ERA5_TOC() %>%
                    ReadNetCDF(vars = c(toc = "tco3"),
                               subset = list(time = main_period)) %>%
                    .[season(time) == "SON"] %>%
                    normalise_coords() %>%
                    na.omit() %>%
                    .[is.full_season(time)] %>%
                    .[lat <= -20] %>%
                    .[, toc := toc/2.1415e-5] %>%    # transform to Dobson Units (https://sacs.aeronomie.be/info/dobson.php)
                    .[, w := season_weights(time)] %>%
                    .[, .(toc = mean(toc*w)), by = .(lon, lat, time = seasonally(time))])
     
     , tar_target(all_sims, cmip_files())
     # CMIP6:
     # Obtener la lista de modelos
     , tar_target(sims, all_sims %>%
                    cmip_remove_bad_sources() %>% 
                    cmip_best_grid() %>% 
                    cmip_filter_complete_historical() %>% 
                    cmip_filter_zero_size() %>% 
                    cmip_filter_incomplete_range() %>% 
                    cmip_select_best_version() %>% 
                    cmip_select_required_variables(required_variables = c("tos", "zg"))
                  # , cue = tar_cue("always")  # uncomment to upate files.
     )
     
     , tar_target(cmip_citations, save_cmip6_citations(sims))
     
     
     , tar_target(files, sims$files, 
                  iteration = "list")
     
     , tar_target(files_latest_modified, vapply(files, last_modified, double(1)),
                  # cue = tar_cue("always"), 
                  iteration = "list")
     
     # Preprocesar los archivos
     # , tar_target(sh_files, eliotesis::cmip_select_SH(files, folder = sh_dir),
     #              pattern = map(files, files_latest_modified),
     #              format = "file_fast")
     
     , tar_target(son_plans, cmip_select_son_plan(files, folder = son_dir), 
                  pattern = map(files, files_latest_modified),
                  iteration = "list"
                  )

     , tar_target(son_files, cmip_apply_plan(son_plans),
                  pattern = map(son_plans, files_latest_modified),
                  format = "file_fast"
                  )
     
     , tar_target(son_files_latest_modified, vapply(son_files, last_modified, double(1)),
                  # cue = tar_cue("always"),
                  iteration = "list")
     
     , tar_target(simulaciones, son_files %>%
                    basename() %>%
                    unglue::unglue_data("{variable}_{mean}_{model}_{experiment}_r{member}i{init}p{physics}f{forcing}_{grid}_{start_date}-{end_date}.nc",
                                        convert = TRUE) %>%
                    data.table::as.data.table() %>%
                    .[, file := son_files] %>%
                    .[mean != "Emon"] %>%
                    .[order(variable, model, member)] %>%
                    .[, member := interaction(member, init, physics, forcing)] %>% 
                    .[, last_modified := son_files_latest_modified]
     )

     # Computar CEOF de ERA5
     , tar_target(ceof_era5, era5[, eliotesis::compute_ceof(hgt, lon, lat, lev, time)] %>%
                    rotate_ceof_era5(toc, enso, o3wave_lats) %>%
                    check_rotation() %>%
                    organise_ceof_era5())

     , tar_target(ref_era5, ceof_era5$eof[[1]]$right %>%
                    copy() %>%
                    setnames("hgt", "eof_ref"))

     , tar_target(simulaciones_group, simulaciones %>%
                    copy() %>%
                    .[, tar_group := target_groups(experiment, model)],
                  iteration = "group")

     , tar_target(ceof_cmip, simulaciones_group %>%
                    .[variable == "zg"] %>%
                    .[, .(eof = list(eliotesis::compute_ceof_cmip(file, member,
                                                                  time = times,
                                                                  ref = ref_era5))),
                      by = .(experiment, model, tar_group)],
                  pattern = map(simulaciones_group))

     , tar_target(ceofs, rbind(ceof_cmip, ceof_era5[, tar_group := max(ceof_cmip$tar_group) + 1]))

     , tar_target(ceof_fiels,
                  ceofs[, denormalise(eof[[1]], "right"), by = .(experiment, model)] %>%
                    sep_ReIm())

     , tar_target(ceof_largo_p, simulaciones_group %>%
                    .[variable == "zg"] %>%
                    .[, eliotesis::ceof_proyectado(file, member, model, experiment,
                                                   fields = ceof_fiels,
                                                   time = c(lubridate::NA_Date_, times[2])),
                      by = .(experiment, model)],
                  pattern = map(simulaciones_group))
     , tar_target(simulaciones_historical, simulaciones_group[experiment == "historical"] %>%
                    .[, tar_group := target_groups(model, member)],
                  iteration = "group")

     , tar_target(enso_cmip, simulaciones_historical %>%
                    .[variable == "tos"] %>%
                    .[, compute_enso_cmip(file), by = .(model, experiment, member)],
                  pattern = map(simulaciones_historical))

     , tar_target(sst_regr_era5, regress_ceof_sst_era5(ceof_era5$eof[[1]], sst))

     # , tar_target(sst_regr_cmip6, regress_sst(simulaciones_group, ceofs, experiments, models),
     #            pattern = map(simulaciones_group, ceofs))
     
)




# Calcular cEOF de ERA5


# Calcular ASAM de ERA5

