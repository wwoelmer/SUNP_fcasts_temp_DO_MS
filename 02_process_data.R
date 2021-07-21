# 
# lake_directory <- getwd()
# config <- yaml::read_yaml(file.path(lake_directory, "configuration", "FLAREr", "configure_flare.yml"))
# 
# # Set working directories for your system
 config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
 config$file_path$data_directory <- file.path(lake_directory, "data_raw")
 config$file_path$noaa_directory <- file.path(lake_directory, "forecasted_drivers")
# 
# # set up run config settings
# run_config <- yaml::read_yaml(file.path(lake_directory,"configuration", "FLAREr", "configure_run.yml"))
# config$run_config <- run_config
# 
# # set forecast model info
 if(config$run_config$forecast_horizon==16){
   config$met$forecast_met_model <-'noaa-point/NOAAGEFS_1hr'
 }else if(config$run_config$forecast_horizon==35){
   config$met$forecast_met_model <- 'noaa/NOAAGEFS_1hr'
 }


library(tidyverse)
library(lubridate)


# get NOAA met forecasts and stack first day to use as met 'obs'
source(file.path(lake_directory, "R", "stack_noaa_forecasts_cycle_loop.R"))
dates <- seq.Date(as.Date('2021-05-26'), as.Date(config$run_config$forecast_start_datetime), by = 'day') # cycle through historical dates 
cycle <- '00'
outfile <- config$file_path$qaqc_data_directory
stack_noaa_forecasts(dates = dates, 
                     #cycle = cycle, 
                     outfile = outfile, 
                     config = config)


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
