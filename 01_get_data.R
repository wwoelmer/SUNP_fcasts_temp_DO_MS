# Script to download the latest observed water quality and met data from Sunapee buoy
# as well as NOAA forecasts for the forecast time period

#remotes::install_github("FLARE-forecast/FLAREr")
# library(FLAREr)

############## set up config directories
lake_directory <- getwd() # Captures the project directory 
config <- yaml::read_yaml(file.path(lake_directory,"configuration", "FLAREr", "configure_flare.yml"))

# Set working directories for your system
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config$file_path$noaa_directory <- file.path(lake_directory, "forecasted_drivers", config$met$forecast_met_model)
config_obs <- yaml::read_yaml(file.path(lake_directory,"configuration", "observation_processing", "observation_processing.yml"))


# download buoy data, water quality and met
setwd(file.path(config$file_path$data_directory, config_obs$realtime_insitu_location))
system("git pull")
# setwd("C:/Users/wwoel/Desktop/SUNP-forecast") # NO HARDCODED PATHS!!!! Antithesis of reproducible workflows 
setwd("../../") # Either use relative paths or lake_directory which is defined above!
setwd(lake_directory)

# download NOAA data
# source a function here

source(file.path(lake_directory, "R", "noaa_download_s3.R"))

# set a start and end date for NOAA forecasts and check which days are not available in local NOAA directory
dates <- seq.Date(as.Date('2021-05-22'), as.Date(Sys.Date()), by = 'day')
download_dates <- c()
for (i in 1:length(dates)) {
  fpath <- file.path(config$file_path$noaa_directory, "NOAAGEFS_1hr", "sunp", dates[i])
  if(dir.exists(fpath)){
    message(paste0(dates[i], ' already downloaded'))
  }else{
    download_dates <- c(download_dates, dates[i])
  }
}

download_dates <- na.omit(download_dates)
download_dates <- as.Date(download_dates, origin = '1970-01-01')


for (i in 1:length(download_dates)) {
  noaa_download_s3(siteID = 'sunp',
                   date = download_dates[i],
                   cycle = '00',
                   noaa_horizon = 16,
                   noaa_directory = config$file_path$noaa_directory)
  
}

# noaa-point is 16 day

