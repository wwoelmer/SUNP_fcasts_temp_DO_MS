# run average stacking function and look at output
library(tidyverse)

lake_directory <- getwd()
forecast_dates <- seq.Date(as.Date('2021-06-02'), as.Date('2021-07-02'), by = 'day') # cycle through historical dates 
site <- "sunp" #four digit name in lowercase
noaa_directory <- file.path(lake_directory, "forecasted_drivers", "noaa")
#noaa_model <- "NOAAGEFS_6hr" 
output_directory <- file.path(lake_directory, "data_processed") # file path where you want the output file to go
outfile_name = "observed-met-noaa" # file name prefix of the output
noaa_hour = 1# numeric, whether you w

config <- yaml::read_yaml(file.path(lake_directory,"configuration", "FLAREr", "configure_flare.yml"))

# Set working directories for your system
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config$file_path$noaa_directory <- file.path(lake_directory, "forecasted_drivers")
config_obs <- yaml::read_yaml(file.path(lake_directory,"configuration", "observation_processing", "observation_processing.yml"))

# set up run config settings
run_config <- yaml::read_yaml(file.path(lake_directory,"configuration", "FLAREr", "configure_run.yml"))
config$run_config <- run_config

dat <- ncdf4::nc_open(file.path(config$file_path$qaqc_data_directory, "observed-met-noaa_sunp.nc"))
cf_met_vars <- c("air_temperature",
                 "surface_downwelling_shortwave_flux_in_air",
                 "surface_downwelling_longwave_flux_in_air",
                 "relative_humidity",
                 "wind_speed",
                 "precipitation_flux")
glm_met_vars <- c("AirTemp",
                  "ShortWave",
                  "LongWave",
                  "RelHum",
                  "WindSpeed",
                  "Rain")

obs_met_time <- ncdf4::ncvar_get(dat, "time")
origin <- stringr::str_sub(ncdf4::ncatt_get(dat, "time")$units, 13, 28)
origin <- lubridate::ymd_hm(origin)
obs_met_time <- origin + lubridate::hours(obs_met_time)
met <- tibble::tibble(time = obs_met_time)
met <- tibble::tibble(time = obs_met_time)

for(i in 1:length(cf_met_vars)){
  
  met <- cbind(met, ncdf4::ncvar_get(dat, cf_met_vars[i]))
}

ncdf4::nc_close(dat)
names(met) <- c("time", glm_met_vars)
met$AirTemp <- met$AirTemp - 273

plot(met$time, met$AirTemp, type = 'l')
plot(met$time, met$ShortWave, type = 'l')
met <- met %>% 
  distinct(time, .keep_all = TRUE)
#met <- met %>% pivot_longer(cols = glm_met_vars, names_to = 'variable', values_to = 'value')

buoy_met_file <- file.path(lake_directory, "data_raw", "buoy-data", "SUNP_buoy_met.csv")
d_head<-read.csv(buoy_met_file, skip=1, as.is=T) #get header minus wonky Campbell rows
d <-read.csv(buoy_met_file, skip=4, header=F) #get data minus wonky Campbell rows
names(d)<-names(d_head) #combine the names to deal with Campbell logger formatting

#Removes row if the TIMESTAMP column is blank
d <- d[complete.cases(d$TIMESTAMP),]

#Removes row if the RECORD column has an NA or blank
d <- d[!(is.na(d$RECORD) | d$RECORD==""), ]

# convert to UTC
#d$TIMESTAMP <- as.POSIXct(d$TIMESTAMP)
#attr(d$TIMESTAMP, "tzone") <- "UTC"
# convert to UTC
d$TIMESTAMP <- as.POSIXct(d$TIMESTAMP, tz = "UTC")
#d$TIMESTAMP <- d$TIMESTAMP + lubridate::hours(1) # Add the 5 hours to EST to make it UTC

obs <- d %>% 
  select(TIMESTAMP, PAR_Den_Avg, AirTC, RH, mean_wind_speed_vector) %>% 
  rename(time = TIMESTAMP, 
         PAR = PAR_Den_Avg, 
         AirTemp = AirTC, 
         RelHum = RH,  
         WindSpeed = mean_wind_speed_vector) %>% 
  mutate(date_hour  = paste0(lubridate::date(time), " ", lubridate::hour(time), ":00:00")) %>% 
  group_by(date_hour) %>% 
  mutate(PAR = mean(PAR),
         AirTemp = mean(AirTemp),
         RelHum = mean(RelHum),
         WindSpeed = mean(WindSpeed)) %>% 
  distinct(date_hour, .keep_all = TRUE) %>% 
  select(date_hour, PAR:WindSpeed, -time)
colnames(obs)[1] <- "time"
obs$time <- as.POSIXct(obs$time, tz = "UTC")
attr(obs$time, "tzone") <- "UTC"
#plot(obs$time, obs$AirTemp, type = 'l')
#points(met$time, met$AirTemp, col = 'red', type = 'l')
obs <- obs[obs$time <= max(met$time),]
met <- met[met$time <= max(obs$time),]
met <- met[met$time >= min(obs$time),]

library(LakeMetabolizer)
obs <- par.to.sw(obs, par.col = 'PAR', coeff=0.473)

plot(met$time, met$AirTemp, type = 'l')
points(obs$time, obs$AirTemp, type = 'l', col = 'red')

plot(met$time, met$WindSpeed, type = 'l', ylim = c(0, 8))
points(obs$time, obs$WindSpeed, type = 'l', col = 'red')

plot(met$time, met$RelHum, type = 'l')
points(obs$time, obs$RelHum/100, type = 'l', col = 'red')

plot(met$time, met$ShortWave, type = 'l')
points(obs$time, obs$sw, type = 'l', col = 'red')

plot(obs$sw, met$ShortWave)
# how accurate is the par -> conversion? how accurate is the par sensor?
plot(obs$AirTemp, met$AirTemp)
plot(obs$RelHum/100, met$RelHum)
plot(obs$WindSpeed, met$WindSpeed)

summary(lm(obs$AirTemp ~ met$AirTemp))
summary(lm(obs$RelHum ~ met$RelHum))
summary(lm(obs$WindSpeed ~ met$WindSpeed))
