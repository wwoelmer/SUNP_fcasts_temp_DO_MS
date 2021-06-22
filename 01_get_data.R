# Script to download the latest observed water quality and met data from Sunapee buoy
# as well as NOAA forecasts for the forecast time period

#remotes::install_github("FLARE-forecast/FLAREr")
library(FLAREr)

############## set up config directories
lake_directory <- getwd()
config <- yaml::read_yaml(file.path(lake_directory,"configuration", "FLAREr", "configure_flare.yml"))
config_obs <- yaml::read_yaml(file.path(lake_directory,"configuration", "observation_processing", "observation_processing.yml"))


# download buoy data, water quality and met
setwd(file.path(config$file_path$data_directory, config_obs$realtime_insitu_location))
system("git pull")
setwd("C:/Users/wwoel/Desktop/SUNP-forecast")

# download NOAA data
# source a function here

source(paste0(config$file_path$script_directory, "/noaa_download_s3.R"))

# set a start and end date for NOAA forecasts and check which days are not available in local NOAA directory
dates <- seq.Date(as.Date('2021-05-22'), as.Date(Sys.Date()), by = 'day')
download_dates <- array(NA)
path_dates <- array(NA, length(dates))
for (i in 1:length(dates)) {
  path_dates[i] <- paste0(config$file_path$noaa_directory, "drivers/noaa-point/NOAAGEFS_1hr/sunp/", dates[i])
  if(file.exists(path_dates[i])){
    print(paste0(dates[i], ' already downloaded'))
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

