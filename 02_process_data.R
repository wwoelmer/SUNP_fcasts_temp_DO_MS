
config$file_path$data_directory 
config$file_path$qaqc_data_directory
#config$obs_config <- readr::read_csv(file.path(run_config$forecast_location, config$obs_config_file), col_types = readr::cols())

library(tidyverse)
library(lubridate)


# get NOAA met forecasts and stack first day to use as met 'obs'
source(paste0(config$file_path$script_directory, "/stack_noaa_forecasts.R"))
dates <- seq.Date(as.Date('2021-05-23'), as.Date('2021-06-21'), by = 'day') # cycle through historical dates 
cycle <- '00'
outfile <- paste0(config$file_path$qaqc_data_directory)
stack_noaa_forecasts(dates = dates, cycle = cycle, outfile = outfile)


# QAQC insitu buoy data
source(paste0(config$file_path$script_directory, "/buoy_wq_qaqc.R"))
realtime_file <- paste0(config$file_path$data_directory, "/", config_obs$insitu_obs_fname[1])
hist_file <- paste0(config$file_path$data_directory, "/", config_obs$insitu_obs_fname[2])
maintenance_url <- 'https://docs.google.com/spreadsheets/d/1IfVUlxOjG85S55vhmrorzF5FQfpmCN2MROA_ttEEiws/edit?usp=sharing'
variables <- c('temperature') # list of WQ variables to include in FLARE notation, e.g., temperature, oxygen, chla, fdom
config <- config

insitu_qaqc(realtime_file = realtime_file,
            hist_file = hist_file,
            maintenance_url = maintenance_url,
            variables = variables,
            config = config)
