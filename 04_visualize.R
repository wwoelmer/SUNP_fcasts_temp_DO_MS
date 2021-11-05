renv::restore()

library(tidyverse)
library(lubridate)

lake_directory <- here::here()

source(file.path(lake_directory,"R/post_forecast_functions/plotting.R"))

s3_mode <- TRUE

run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
forecast_site <- run_config$forecast_site
config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "configure_flare_",forecast_site,".yml")))

#Get updated run_config from bucket if one exists
if(s3_mode){
  restart_exists <- aws.s3::object_exists(object = file.path(forecast_site, "configure_run.yml"), bucket = "restart")
  if(restart_exists){
    aws.s3::save_object(object = file.path(forecast_site, "configure_run.yml"), bucket = "restart", file = file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
  }
  run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
  forecast_site <- run_config$forecast_site
  run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
  if(!is.na(run_config$restart_file)){
    restart_file <- basename(run_config$restart_file)
  }else{
    restart_file <- NA
  }
  if(!is.na(restart_file)){
    aws.s3::save_object(object = file.path(forecast_site, restart_file),
                        bucket = "forecasts",
                        file = file.path(lake_directory, "forecasts", restart_file))
    restart_file <- basename(run_config$restart_file)
    config$run_config$restart_file <- file.path(lake_directory, "forecasts", restart_file)
  }
  if(!is.na(run_config$restart_file)){
    config$run_config$restart_file <- file.path(lake_directory, "forecasts", restart_file)
  }
  config$run_config <- run_config
}else{
  if(!is.na(run_config$restart_file)){
    file.copy(from = run_config$restart_file, to = config$file_path$forecast_output_directory)
  }
}

target_directory <- file.path(lake_directory, "data_processed")

if(s3_mode){
  aws.s3::save_object(object = file.path(forecast_site, paste0(forecast_site, "-targets-insitu.csv")),
                      bucket = "targets",
                      file = file.path(target_directory, paste0(forecast_site, "-targets-insitu.csv")))
}

pdf_file <- FLAREr::plotting_general_2(file_name = config$run_config$restart_file,
                                       target_file = file.path(target_directory, paste0(forecast_site, "-targets-insitu.csv")))


source(file.path(lake_directory,"R/simple_plot.R"))

forecast_file_name <- config$run_config$restart_file
output_file_name <- paste0(config$file_path$forecast_output_directory, "/", config$run_config$sim_name, "_", lubridate::date(config$run_config$forecast_start_datetime), "_", config$run_config$forecast_horizon, '_day_forecast_simple_plot' )
qaqc_data_directory <- config$file_path$qaqc_data_directory
focal_depths_plotting <- c('0.1', '5', '10')
highlight_date <- as.Date('2021-07-17')

simple_plot(forecast_file_name,
            output_file_name,
            qaqc_data_directory,
            focal_depths_plotting,
            highlight_date = highlight_date)


