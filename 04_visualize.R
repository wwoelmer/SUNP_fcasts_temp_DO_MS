
library(tidyverse)
library(lubridate)

Sys.setenv("AWS_DEFAULT_REGION" = "s3",
           "AWS_S3_ENDPOINT" = "flare-forecast.org")

lake_directory <- here::here()

configure_run_file <- "configure_run.yml"

config <- FLAREr::set_configuration(configure_run_file,lake_directory)

config <- FLAREr::get_restart_file(config, lake_directory)

FLAREr::get_targets(lake_directory, config)

pdf_file <- FLAREr::plotting_general_2(file_name = config$run_config$restart_file,
                                       target_file = file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv")))

if(config$run_config$use_s3){
  success <- aws.s3::put_object(file = pdf_file, object = file.path(config$location$site_id, basename(pdf_file)), bucket = "analysis")
  if(success){
    unlink(pdf_file)
  }
}

source(file.path(lake_directory,"R/simple_plot.R"))

forecast_file_name <- config$run_config$restart_file
output_file_name <- paste0(config$file_path$forecast_output_directory, "/", config$run_config$sim_name, "_", lubridate::date(config$run_config$forecast_start_datetime), "_", config$run_config$forecast_horizon, '_day_forecast_simple_plot' )
qaqc_data_directory <- config$file_path$qaqc_data_directory
focal_depths_plotting <- c('0.1', '5', '10')
highlight_date <- as.Date('2021-07-17')

#simple_plot(forecast_file_name,
#            output_file_name,
#            qaqc_data_directory,
#            focal_depths_plotting,
#            highlight_date = highlight_date)

if(config$run_config$use_s3){
  unlink(file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv")))
  unlink(config$run_config$restart_file)
}


