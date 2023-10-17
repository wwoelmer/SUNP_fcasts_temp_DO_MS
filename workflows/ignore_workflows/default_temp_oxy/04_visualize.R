
library(tidyverse)
library(lubridate)

lake_directory <- here::here()

configure_run_file <- "configure_run.yml"

config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

config <- FLAREr::get_restart_file(config, lake_directory)

FLAREr::get_targets(lake_directory, config)

pdf_file <- FLAREr::plotting_general_2(file_name =  config$run_config$restart_file,
                                       target_file = file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv")))

if(config$run_config$use_s3){
  success <- aws.s3::put_object(file = pdf_file, object = file.path(config$location$site_id, basename(pdf_file)), bucket = "analysis")
  if(success){
    unlink(pdf_file)
  }
}

source(file.path(lake_directory,"R/simple_plot.R"))

forecast_file_name <- config$run_config$restart_file
output_file_name <- paste0(config$file_path$forecast_output_directory, "/", config$run_config$sim_name, "_", config$run_config$forecast_horizon, 'day_simple_plot_', lubridate::date(config$run_config$forecast_start_datetime))
qaqc_data_directory <- config$file_path$qaqc_data_directory
focal_depths_plotting <- c('0.1', '5', '10')

simple_file_name <- simple_plot(forecast_file_name,
                                output_file_name,
                                qaqc_data_directory,
                                focal_depths_plotting,
                                num_days_plot = 10)


if(config$run_config$use_s3){
  success <- aws.s3::put_object(file = simple_file_name, object = file.path(config$location$site_id, basename(simple_file_name)), bucket = "analysis")
  if(success){
    unlink(simple_file_name)
  }
  unlink(file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv")))
  unlink(config$run_config$restart_file)
}


