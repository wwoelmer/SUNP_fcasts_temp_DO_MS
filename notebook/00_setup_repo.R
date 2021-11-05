# script to set up repository to run FLARE for SUNP

#remotes::install_github("FLARE-forecast/GLM3r")
#remotes::install_github('eco4cast/EFIstandards')
#remotes::install_github("FLARE-forecast/FLAREr")
#install.packages(aws.s3)
#install.packages(tidyverse)

############## set up config directories
lake_directory <- here::here() # Captures the project directory 
config <- yaml::read_yaml(file.path(lake_directory,"configuration", "FLAREr", "configure_flare.yml"))

# Set working directories for your system
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$data_directory <- file.path(lake_directory, "data_raw")



############## clone github repositories
setwd(config$file_path$data_directory)
system("git clone -b sunp-buoy-data --depth 1 https://github.com/FLARE-forecast/SUNP-data.git buoy-data")

############## download historical data
# buoy temp and DO data from 2007 onward
data  <-  "https://pasta.lternet.edu/package/data/eml/edi/499/2/1f903796efc8d79e263a549f8b5aa8a6" # URL from EDI: https://portal.edirepository.org/nis/codeGeneration?packageId=edi.499.2&statisticalFileType=r
try(download.file(data, destfile = file.path(config$file_path$data_directory, "hist-data", "hist_buoy_temp.csv"), 
                  method = "curl"))

data <- "https://pasta.lternet.edu/package/data/eml/edi/499/2/f4d3535cebd96715c872a7d3ca45c196" 
try(download.file(data, destfile = file.path(config$file_path$data_directory, "hist-data", "hist_buoy_do.csv"), method = "curl"))

setwd(lake_directory)
