renv::restore()

#install.packages("gsheet")
library(tidyverse)
library(lubridate)

lake_directory <- here::here()

s3_mode <- TRUE

source(file.path(lake_directory, "R", "s3_functions.R"))
source(file.path(lake_directory, "R", "average_stacked_forecasts.R"))
source(file.path(lake_directory, "R", "insitu_qaqc.R"))

if(file.exists("~/.aws")){
  warning(paste("Detected existing AWS credentials file in ~/.aws,",
                "Consider renaming these so that automated upload will work"))
}

Sys.setenv("AWS_DEFAULT_REGION" = "s3",
           "AWS_S3_ENDPOINT" = "flare-forecast.org")

configuration_file <- "configure_flare.yml"
run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
forecast_site <- run_config$forecast_site

config_obs <- yaml::read_yaml(file.path(lake_directory,"configuration","observation_processing","observation_processing.yml"))
#Note: lake_directory need to be set prior to running this script
config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr",configuration_file))
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config_obs$data_location <- config$file_path$data_directory
config$file_path$noaa_directory <- file.path(dirname(lake_directory), "drivers", "noaa")

setwd(file.path(lake_directory, "data_raw"))
if(!dir.exists(file.path(lake_directory, "data_raw", config_obs$realtime_insitu_location))){
  system(paste0("git clone --depth 1 --single-branch --branch ",config_obs$realtime_insitu_location, " https://github.com/FLARE-forecast/SUNP-data.git ", config_obs$realtime_insitu_location))
}else{
  setwd(file.path(lake_directory, "data_raw", config_obs$realtime_insitu_location))
  system("git pull")
}

if(!dir.exists(dir.create(file.path(config$file_path$data_directory, "hist-data")))){
  dir.create(file.path(config$file_path$data_directory, "hist-data"))
}

if(!file.exists(file.path(config$file_path$data_directory, "hist-data", "hist_buoy_temp.csv"))){
  data  <-  "https://pasta.lternet.edu/package/data/eml/edi/499/2/1f903796efc8d79e263a549f8b5aa8a6" # URL from EDI: https://portal.edirepository.org/nis/codeGeneration?packageId=edi.499.2&statisticalFileType=r
  try(download.file(data, destfile = file.path(config$file_path$data_directory, "hist-data", "hist_buoy_temp.csv"), 
                    method = "curl"))
}

if(!file.exists(file.path(config$file_path$data_directory, "hist-data", "hist_buoy_do.csv"))){
  data <- "https://pasta.lternet.edu/package/data/eml/edi/499/2/f4d3535cebd96715c872a7d3ca45c196" 
  try(download.file(data, destfile = file.path(config$file_path$data_directory, "hist-data", "hist_buoy_do.csv"), method = "curl"))
}


download_s3_objects(lake_directory, bucket = "drivers", prefix = file.path("noaa", "NOAAGEFS_1hr_stacked", forecast_site))

# format the stacked forecasts for input into flare
stack_file_name <- file.path(lake_directory, "data_processed", paste0("observed-met-noaa_",forecast_site, ".nc"))

average_stacked_forecasts(forecast_dates = seq.Date(as_date(run_config$start_datetime), as_date(run_config$forecast_start_datetime), by = 'day'), # cycle through historical dates
                          site = forecast_site, #four digit name in lowercase
                          noaa_hour = 1,
                          noaa_stacked_directory = file.path(lake_directory, "drivers", "noaa", "NOAAGEFS_1hr_stacked"),
                          output_file = stack_file_name)

stack_file_name <- file.path(lake_directory, "data_processed", paste0("observed-met-noaa_",forecast_site, ".nc"))
                          
                          

# QAQC insitu buoy data
insitu_qaqc(realtime_file = file.path(config$file_path$data_directory, config_obs$insitu_obs_fname[1]),
            hist_file =  file.path(config$file_path$data_directory, config_obs$insitu_obs_fname[2]),
            maintenance_url = "https://docs.google.com/spreadsheets/d/1IfVUlxOjG85S55vhmrorzF5FQfpmCN2MROA_ttEEiws/edit?usp=sharing",
            variables = "temperature",
            config = config)

if(s3_mode){
  aws.s3::put_object(file = processed_filename, object = file.path(forecast_site, basename(processed_filename)), bucket = "targets")
  aws.s3::put_object(file = stack_file_name, object = file.path(forecast_site, basename(stack_file_name)), bucket = "targets")
}

