library(tidyverse)
library(lubridate)

if(file.exists("~/.aws")){
  warning(paste("Detected existing AWS credentials file in ~/.aws,",
                "Consider renaming these so that automated upload will work"))
}

lake_directory <- here::here()
update_run_config <- TRUE

configure_run_file <- "configure_run.yml"
config_set_name <- "default_temp_oxy"

config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

config <- FLAREr::get_restart_file(config, lake_directory)

# targets are created in 01 script
#FLAREr::get_targets(lake_directory, config)

noaa_forecast_path <- FLAREr::get_driver_forecast_path(config,
                                               forecast_model = config$met$forecast_met_model)

#inflow_forecast_path <- FLAREr::get_driver_forecast_path(config,
#                                                 forecast_model = config$inflow$forecast_inflow_model)
inflow_forecast_path <- NULL


if(!is.null(noaa_forecast_path)){
  FLAREr::get_driver_forecast(lake_directory, forecast_path = noaa_forecast_path, config)
  forecast_dir <- file.path(config$file_path$noaa_directory, noaa_forecast_path)
}else{
  forecast_dir <- NULL
}

if(!is.null(inflow_forecast_path)){
  FLAREr::get_driver_forecast(lake_directory, forecast_path = inflow_forecast_path, config)
  inflow_file_dir <- file.path(config$file_path$noaa_directory,inflow_forecast_path)
}else{
  inflow_file_dir <- NULL
}

pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

#pars_config <- NULL

#Download and process observations (already done)

FLAREr::get_stacked_noaa(lake_directory, config, averaged = TRUE)
  
  
met_out <- FLAREr::generate_met_files_arrow(obs_met_file = NULL,
                                            out_dir = config$file_path$execute_directory,
                                            start_datetime = config$run_config$start_datetime,
                                            end_datetime = config$run_config$end_datetime,
                                            forecast_start_datetime = config$run_config$forecast_start_datetime,
                                            forecast_horizon =  config$run_config$forecast_horizon,
                                            site_id = config$location$site_id,
                                            use_s3 = TRUE,
                                            bucket = config$s3$drivers$bucket,
                                            endpoint = config$s3$drivers$endpoint,
                                            local_directory = NULL,
                                            use_forecast = TRUE,
                                            use_ler_vars = FALSE,
                                            use_siteid_s3 = TRUE)
#Need to remove the 00 ensemble member because it only goes 16-days in the future
met_out$filenames <- met_out$filenames[!stringr::str_detect(met_out$filenames, "ens00")]

#Create observation matrix
obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long = file.path(config_obs$file_path$targets_directory, config_obs$site_id, config_set_name, paste0(config_obs$site_id,"-targets-insitu.csv")),
                                 obs_config = obs_config,
                                 config)
obs[1, ,]
obs[2, ,]
# dimensions: states, time, depth

states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)

model_sd <- FLAREr::initiate_model_error(config, states_config)

init <- FLAREr::generate_initial_conditions(states_config,
                                            obs_config,
                                            pars_config,
                                            obs,
                                            config,
                                            #restart_file = config$run_config$restart_file,
                                            historical_met_error = met_out$historical_met_error)
#Run EnKF
da_forecast_output <- FLAREr::run_da_forecast(states_init = init$states,
                                              pars_init = init$pars,
                                              aux_states_init = init$aux_states_init,
                                              obs = obs,
                                              obs_sd = obs_config$obs_sd,
                                              model_sd = model_sd,
                                              working_directory = config$file_path$execute_directory,
                                              met_file_names = met_out$filenames,
                                              inflow_file_names = NULL,
                                              outflow_file_names = NULL,
                                              config = config,
                                              pars_config = pars_config,
                                              states_config = states_config,
                                              obs_config = obs_config,
                                              management = NULL,
                                              da_method = config$da_setup$da_method,
                                              par_fit_method = config$da_setup$par_fit_method)

#########################################
# Save files
message("Writing forecast")
saved_file <- FLAREr::write_forecast_netcdf(da_forecast_output = da_forecast_output,
                                            forecast_output_directory = config$file_path$forecast_output_directory,
                                            use_short_filename = TRUE)

forecast_df <- FLAREr::write_forecast_arrow(da_forecast_output = da_forecast_output,
                                            use_s3 = config$run_config$use_s3,
                                            bucket = config$s3$forecasts_parquet$bucket,
                                            endpoint = config$s3$forecasts_parquet$endpoint,
                                            local_directory = file.path(lake_directory, "forecasts/parquet"))

message("Writing arrow score")

message("Grabbing last 16-days of forecasts")
reference_datetime_format <- "%Y-%m-%d %H:%M:%S"
past_days <- strftime(lubridate::as_datetime(forecast_df$reference_datetime[1]) - lubridate::days(config$run_config$forecast_horizon), tz = "UTC")

vars <- FLAREr:::arrow_env_vars()
s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint)
past_forecasts <- arrow::open_dataset(s3) |>
  dplyr::filter(model_id == forecast_df$model_id[1],
                site_id == forecast_df$site_id[1],
                reference_datetime > past_days) |>
  dplyr::collect()
FLAREr:::unset_arrow_vars(vars)

message("Combining forecasts")
combined_forecasts <- dplyr::bind_rows(forecast_df, past_forecasts)

message("Scoring forecasts")
FLAREr::generate_forecast_score_arrow(targets_file = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),
                                      forecast_df = combined_forecasts,
                                      use_s3 = config$run_config$use_s3,
                                      bucket = config$s3$scores$bucket,
                                      endpoint = config$s3$scores$endpoint,
                                      local_directory = file.path(lake_directory, "scores/parquet"),
                                      variable_types = c("state","parameter"))


FLAREr::put_forecast(saved_file, eml_file_name = NULL, config)

rm(da_forecast_output)
gc()

FLAREr::update_run_config(config, lake_directory, configure_run_file, saved_file, new_horizon = 35, day_advance = 1)

setwd(lake_directory)
unlink(config$run_config$restart_file)
unlink(forecast_dir, recursive = TRUE)
unlink(file.path(lake_directory, "flare_tempdir", config$location$site_id, config$run_config$sim_name), recursive = TRUE)


message(paste0("successfully generated flare forecats for: ", basename(saved_file)))


