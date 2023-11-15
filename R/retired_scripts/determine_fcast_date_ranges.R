# identify date ranges for all forecast years
library(tidyverse)
library(lubridate)

lake_directory <- here::here()
config_obs <- FLAREr::initialize_obs_processing(lake_directory, observation_yml = "observation_processing.yml", config_set_name = "UC_analysis")

############################################################
# 2019
f <- read.csv(file.path(config_obs$file_path$targets_directory, config_obs$site_id, "UC_analysis", paste0(config_obs$site_id,"-targets-insitu.csv")))
# limit to first full month when sensors were out in the first year of this study (2019)
f <- f %>% 
  filter(time > as.POSIXct('2019-01-01 00:00:00') & time < as.POSIXct('2019-12-01 00:00:00'))
# NOAA forecasts don't start until '2019-07-09'
spin_up <- seq.Date(as.Date('2019-07-09'), as.Date('2019-07-09') + months(1) + lubridate::days(5), by = "day")  
spin_up
fcasts <- seq.Date((last(spin_up) + days(1)), as.Date(max(f$time)), by = "day")
fcasts
length(fcasts)

############################################################
# 2020
f <- read.csv(file.path(config_obs$file_path$targets_directory, config_obs$site_id, "UC_analysis", paste0(config_obs$site_id,"-targets-insitu.csv")))
# limit to first full month when sensors were out in the first year of this study (2019)
f <- f %>% 
  filter(time > as.POSIXct('2020-01-01 00:00:00') & time < as.POSIXct('2020-12-01 00:00:00'))
min(f$time)
max(f$time)
spin_up <- seq.Date(as.Date(min(f$time)), as.Date(min(f$time)) + months(1) + lubridate::days(5), by = "day")  
spin_up
fcasts <- seq.Date((last(spin_up) + days(1)), as.Date(max(f$time)) - days(35), by = "day")
fcasts
length(fcasts)


############################################################
# 2021
f <- read.csv(file.path(config_obs$file_path$targets_directory, config_obs$site_id, 'UC_analysis', paste0(config_obs$site_id,"-targets-insitu.csv")))
# limit to first full month when sensors were out in the first year of this study (2019)
f <- f %>% 
  filter(time > as.POSIXct('2021-01-01 00:00:00') & time < as.POSIXct('2021-12-01 00:00:00'))
min(f$time)
max(f$time)
# bouy was out, but oxygen sensor wasn't in until later, 06-30
spin_up <- seq.Date(as.Date('2021-06-30'), as.Date('2021-06-30') + months(1) + lubridate::days(5), by = "day")  
spin_up
fcasts <- seq.Date(as.Date((last(spin_up) + days(1))), as.Date(max(f$time))- days(35), by = "day")
fcasts
length(fcasts) 


############################################################
# 2022
f <- read.csv(file.path(config_obs$file_path$targets_directory, config_obs$site_id, "UC_analysis", paste0(config_obs$site_id,"-targets-insitu.csv")))
# limit to first full month when sensors were out in the first year of this study (2019)
f <- f %>% 
  filter(time > as.POSIXct('2022-01-01 00:00:00') & time < as.POSIXct('2022-12-01 00:00:00'))
spin_up <- seq.Date(as.Date('2022-04-27'), as.Date('2022-04-27') + months(1) + lubridate::days(5), by = "day")  
spin_up
fcasts <- seq.Date((last(spin_up) + days(1)), as.Date('2022-10-17') - days(35), by = "day")
fcasts
length(fcasts)
