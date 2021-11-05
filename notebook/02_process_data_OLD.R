# 
lake_directory <- here::here()
# config <- yaml::read_yaml(file.path(lake_directory, "configuration", "FLAREr", "configure_flare.yml"))
# 
# # Set working directories for your system
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config$file_path$noaa_directory <- file.path(dirname(lake_directory), "drivers", "noaa")
forecast_site <- 'sunp'

# 
# # set up run config settings
 run_config <- yaml::read_yaml(file.path(lake_directory,"configuration", "FLAREr", "configure_run.yml"))
 config$run_config <- run_config
 config$met$forecast_met_model <- 'NOAAGEFS_1hr'
 
config_obs <- yaml::read_yaml(file.path(lake_directory,"configuration", "observation_processing", "observation_processing.yml"))
# 
# # set forecast model info
# if(config$run_config$forecast_horizon==16){
#   config$met$forecast_met_model <-'noaa-point/NOAAGEFS_1hr'
# }else if(config$run_config$forecast_horizon==35){
#
# }


library(tidyverse)
library(lubridate)
library(noaaGEFSpoint)


# get NOAA met forecasts and stack first day to use as met 'obs'
forecast_dates <- seq.Date(as.Date('2021-06-02'), as.Date('2021-07-06'), by = 'day') # cycle through historical dates 


#noaa_model <- 'noaa/NOAAGEFS_6hr'
#output_directory <- file.path(config$file_path$noaa_directory)
#model_name <- 'NOAAGEFS_6hr'
#dates_w_errors <- c(as.Date('2021-06-01'), as.Date('2021-06-24'), as.Date('2021-07-03'))
#stack_noaa_forecasts(forecast_dates,
#                     site,
#                     noaa_directory = noaa_directory,
#                     noaa_model = noaa_model,
#                     output_directory = output_directory,
#                     dates_w_errors = dates_w_errors
#                     )

# format the stacked forecasts for input into flare
source(file.path(lake_directory, "R", "average_stacked_forecasts.R"))
average_stacked_forecasts(forecast_dates = seq.Date(as.Date(config$run_config$start_datetime), as.Date(run_config$forecast_start_datetime), by = 'day'), # cycle through historical dates
                          site = forecast_site, #four digit name in lowercase
                          noaa_stacked_directory = file.path(dirname(lake_directory), "drivers", "noaa", "NOAAGEFS_1hr_stacked"),
                          output_directory = file.path(lake_directory, "data_processed"),
                          outfile_name = paste0("observed-met-noaa_",forecast_site),
                          noaa_hour = 1)


# QAQC insitu buoy data
source(file.path(lake_directory, "R", "insitu_qaqc.R"))
realtime_file <- file.path(config$file_path$data_directory, config_obs$insitu_obs_fname[1])
hist_file <- file.path(config$file_path$data_directory, config_obs$insitu_obs_fname[2])
maintenance_url <- 'https://docs.google.com/spreadsheets/d/1IfVUlxOjG85S55vhmrorzF5FQfpmCN2MROA_ttEEiws/edit?usp=sharing'
variables <- c('temperature') # list of WQ variables to include in FLARE notation, e.g., temperature, oxygen, chla, fdom
config <- config

# install.packages("gsheet")
insitu_qaqc(realtime_file = realtime_file,
            hist_file = hist_file,
            maintenance_url = maintenance_url,
            variables = variables,
            config = config)
