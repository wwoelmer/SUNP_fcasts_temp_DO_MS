# Script to download the latest observed water quality and met data from Sunapee buoy
# as well as NOAA forecasts for the forecast time period

#remotes::install_github("FLARE-forecast/FLAREr")
library(FLAREr)
############## set up config directories
lake_directory <- getwd() # Captures the project directory 
config <- yaml::read_yaml(file.path(lake_directory,"configuration", "FLAREr", "configure_flare.yml"))

# Set working directories for your system
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config$file_path$noaa_directory <- file.path(lake_directory, "forecasted_drivers")
config_obs <- yaml::read_yaml(file.path(lake_directory,"configuration", "observation_processing", "observation_processing.yml"))

# set up run config settings
run_config <- yaml::read_yaml(file.path(lake_directory,"configuration", "FLAREr", "configure_run.yml"))
config$run_config <- run_config

# download buoy data, water quality and met
setwd(file.path(config$file_path$data_directory, config_obs$realtime_insitu_location))
system("git pull")
setwd("../../") # Either use relative paths or lake_directory which is defined above!
setwd(lake_directory)

# download NOAA data
# source a function here
#config$file_path$noaa_directory <- file.path(lake_directory, "forecasted_drivers", "cibr_bucket")

source(file.path(lake_directory, "R", "noaa_download_s3.R"))

#if(config$run_config$forecast_horizon==16){
#  config$met$forecast_met_model <-'noaa-point/NOAAGEFS_1hr'
#}else if(config$run_config$forecast_horizon==35){
#  config$met$forecast_met_model <- 'noaa/NOAAGEFS_1hr'
#}
# set a start and end date for NOAA forecasts and check which days are not available in local NOAA directory
#dates <- seq.Date(as.Date('2021-05-26'), as.Date(Sys.Date()), by = 'day')
dates <- seq.Date(as.Date('2021-06-01'), as.Date('2021-07-07'), by = 'day')
#download_dates <- c()
for (i in 1:length(dates)) {
  fpath <- file.path(config$file_path$noaa_directory, config$met$forecast_met_model, "sunp", dates[i])
  if(dir.exists(fpath)){
    message(paste0(dates[i], ' already downloaded'))
  }else{
    download_dates <- c(download_dates, dates[i])
  }
}
download_dates <- dates

download_dates <- na.omit(download_dates)
download_dates <- as.Date(download_dates, origin = '1970-01-01')
#download_dates <-seq.Date(as.Date('2021-05-22'), as.Date(Sys.Date()), by = 'day')
cycle <- c('06', '12', '18')

if(length(download_dates>1)){
  for (i in 1:length(download_dates)) {
    for(j in 1:length(cycle)){
    noaa_download_s3(siteID = 'sunp',
                     date = download_dates[i],
                     cycle = cycle[j],
                     noaa_horizon = config$run_config$forecast_horizon,
                     noaa_directory = file.path(config$file_path$noaa_directory, config$met$forecast_met_model),
                     noaa_model = config$met$forecast_met_model,
                     noaa_hour = 6)
    }
    
  }
  
}

library(aws.s3)
df <- get_bucket_df(bucket = "flare", prefix = "drivers/noaa/NOAAGEFS_6hr", region = "", max = Inf)


