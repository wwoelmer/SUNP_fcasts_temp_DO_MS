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

# download NOAA data
# source a function here

source(paste0(config$file_path$script_directory, "/noaa_download_s3.R"))

noaa_download_s3(siteID = 'fcre',
                 date = "2021-06-16",
                 cycle = '00',
                 noaa_horizon = 16,
                 noaa_directory = config$file_path$noaa_directory)



