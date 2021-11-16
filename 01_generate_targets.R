library(tidyverse)
library(lubridate)

message("Beginning generate targets")

#' Set the lake directory to the repository directory

lake_directory <- here::here()

Sys.setenv("AWS_DEFAULT_REGION" = "s3",
           "AWS_S3_ENDPOINT" = "flare-forecast.org")

#' Source the R files in the repository

source(file.path(lake_directory, "R", "workflow_functions.R"))
source(file.path(lake_directory, "R", "insitu_qaqc.R"))

files.sources <- list.files(file.path(lake_directory, "R"), full.names = TRUE)
sapply(files.sources, source)

#' Generate the `config_obs` object and create directories if necessary

config_obs <- FLAREr::initialize_obs_processing(lake_directory, observation_yml = "observation_processing.yml")
use_s3 <- TRUE

#' Clone or pull from data repositories

FLAREr::get_git_repo(lake_directory,
             directory = config_obs$realtime_insitu_location,
             git_repo = "https://github.com/FLARE-forecast/SUNP-data.git")

#' Download files from EDI
#' 

if(!dir.exists(dir.create(file.path(config$file_path$data_directory, "hist-data")))){
  dir.create(file.path(config$file_path$data_directory, "hist-data"))
}

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/499/2/f4d3535cebd96715c872a7d3ca45c196",
             file = file.path("hist-data", "hist_buoy_do.csv"),
             lake_directory)

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/499/2/1f903796efc8d79e263a549f8b5aa8a6",
             file = file.path("hist-data", "hist_buoy_temp.csv"),
             lake_directory)


#' Clean up insitu

# QAQC insitu buoy data
cleaned_insitu_file <- insitu_qaqc(realtime_file = file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[1]),
            hist_file =  file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[2]),
            maintenance_url = "https://docs.google.com/spreadsheets/d/1IfVUlxOjG85S55vhmrorzF5FQfpmCN2MROA_ttEEiws/edit?usp=sharing",
            variables = "temperature",
            cleaned_insitu_file = file.path(config_obs$file_path$targets_directory, config_obs$site_id, paste0(config_obs$site_id,"-targets-insitu.csv")),
            config = config)

#' Move targets to s3 bucket

message("Successfully generated targets")

FLAREr::put_targets(site_id = config_obs$site_id,
            cleaned_insitu_file,
            cleaned_met_file,
            cleaned_inflow_file,
            use_s3)

message("Successfully moved targets to s3 bucket")

