# script to set up and run FLARE for SUNP

#remotes::install_github("FLARE-forecast/FLAREr")
library(FLAREr)

############## set up config directories
lake_directory <- getwd()
config <- yaml::read_yaml(file.path(lake_directory,"configuration", "FLAREr", "configure_flare.yml"))
config_obs <- yaml::read_yaml(file.path(lake_directory,"configuration", "observation_processing", "observation_processing.yml"))

############## clone github repositories
setwd(config$file_path$data_directory)
system("git clone -b sunp-buoy-data --depth 1 https://github.com/FLARE-forecast/SUNP-data.git buoy-data")

############## download historical data
# buoy data from 2007 onward
data  <-  "https://pasta.lternet.edu/package/data/eml/edi/499/2/1f903796efc8d79e263a549f8b5aa8a6" # URL from EDI: https://portal.edirepository.org/nis/codeGeneration?packageId=edi.499.2&statisticalFileType=r
destination <- config$file_path$data_directory # some location on your computer
try(download.file(data,destfile = paste0(destination, '/hist-data/hist_buoy_temp.csv'),method="curl"))

data <- "https://pasta.lternet.edu/package/data/eml/edi/499/2/f4d3535cebd96715c872a7d3ca45c196" 
destination <- config$file_path$data_directory # some location on your computer
try(download.file(data,destfile = paste0(destination, '/hist-data/hist_buoy_do.csv'),method="curl"))


