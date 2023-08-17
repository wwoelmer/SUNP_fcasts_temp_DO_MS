library(tidyverse)
library(lubridate)

message("Beginning generate targets")

#' Set the lake directory to the repository directory

lake_directory <- here::here()
config_set_name <- "default_temp_oxy"

#' Source the R files in the repository
source(file.path(lake_directory, "R", "insitu_qaqc_withDO.R"))

#' Generate the `config_obs` object and create directories if necessary

config_obs <- FLAREr::initialize_obs_processing(lake_directory, observation_yml = "observation_processing.yml")
config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

#' Clone or pull from data repositories

FLAREr::get_git_repo(lake_directory,
                     directory = config_obs$realtime_insitu_location,
                     git_repo = "https://github.com/FLARE-forecast/SUNP-data.git")

#' Download files from EDI and Zenodo
#' 

dir.create(file.path(config_obs$file_path$data_directory, "hist-data"),showWarnings = FALSE)

# high frequency buoy data
FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/499/2/f4d3535cebd96715c872a7d3ca45c196",
                     file = file.path("hist-data", "hist_buoy_do.csv"),
                     lake_directory)

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/499/2/1f903796efc8d79e263a549f8b5aa8a6",
                     file = file.path("hist-data", "hist_buoy_temp.csv"),
                     lake_directory)

# manually collected data
if(!file.exists(file.path(lake_directory, 'data_raw', 'hist-data', 'LMP-v2023.1.zip'))){
  download.file(url = 'https://zenodo.org/record/7559434/files/Lake-Sunapee-Protective-Association/LMP-v2023.1.zip?download=1',
                destfile = file.path(lake_directory, 'data_raw', 'hist-data', 'LMP-v2023.1.zip'),
                mode = 'wb')
  unzip(file.path(lake_directory, 'data_raw', 'hist-data', 'LMP-v2023.1.zip'),
        files = file.path('Lake-Sunapee-Protective-Association-LMP-d34c6a0', 'primary files', 'LSPALMP_1986-2022_v2023-01-22.csv'),
        exdir = file.path(lake_directory, 'data_raw', 'hist-data', 'LSPA_LMP'),
        junkpaths = TRUE)
}

#' Clean up insitu

# QAQC insitu buoy data
cleaned_insitu_file <- insitu_qaqc(realtime_file = file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[1]),
                                   hist_buoy_file = c(file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[2]), file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[5])),
                                   hist_manual_file = file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[3]),
                                   hist_all_file =  file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[4]),
                                   maintenance_url = "https://docs.google.com/spreadsheets/d/1IfVUlxOjG85S55vhmrorzF5FQfpmCN2MROA_ttEEiws/edit?usp=sharing",
                                   variables = c("temperature", "oxygen"),
                                   cleaned_insitu_file = file.path(config_obs$file_path$targets_directory, config_obs$site_id, config_set_name, paste0(config_obs$site_id,"-targets-insitu.csv")),
                                   config = config_obs,
                                   lake_directory = lake_directory)

#' Move targets to s3 bucket

message("Successfully generated targets")

FLAREr::put_targets(site_id = config_obs$site_id,
                    cleaned_insitu_file,
                    use_s3 = config$run_config$use_s3,
                    config = config)

message("Successfully moved targets to s3 bucket")
