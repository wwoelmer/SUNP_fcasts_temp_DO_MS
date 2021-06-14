

observed_met_file <- file.path(config$qaqc_data_location,"observed-met_fcre.nc") # need met_qaqc to produce .nc file


met_out <- flare::generate_glm_met_files(obs_met_file = observed_met_file,
                                         out_dir = config$run_config$execute_location,
                                         forecast_dir = noaa_forecast_path, 
                                         local_tzone = config$local_tzone,
                                         start_datetime_local = start_datetime_local,
                                         end_datetime_local = end_datetime_local,
                                         forecast_start_datetime = forecast_start_datetime_local,
                                         use_forecasted_met = FALSE)