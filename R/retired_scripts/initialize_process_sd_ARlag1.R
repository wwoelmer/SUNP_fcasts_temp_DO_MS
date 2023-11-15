# script to create an AR lag 1 model to determine sd of residuals for initializing
# process sd

library(arrow)
library(tidyverse)

lake_directory <- here::here()

# read in observations and run qaqc
config_obs <- FLAREr::initialize_obs_processing(lake_directory, observation_yml = "observation_processing.yml", config_set_name = "UC_analysis_2021")
source(file.path(lake_directory, "R", "insitu_qaqc_withDO.R"))

cleaned_insitu_file <- insitu_qaqc(realtime_file = file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[1]),
                                   hist_buoy_file = c(file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[2]), file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[5])),
                                   hist_manual_file = file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[3]),
                                   hist_all_file =  file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[4]),
                                   maintenance_url = "https://docs.google.com/spreadsheets/d/1IfVUlxOjG85S55vhmrorzF5FQfpmCN2MROA_ttEEiws/edit?usp=sharing",
                                   variables = c("temperature", "oxygen"),
                                   cleaned_insitu_file = file.path(config_obs$file_path$targets_directory, config_obs$site_id, paste0(config_obs$site_id,"-targets-insitu.csv")),
                                   config = config_obs,
                                   lake_directory = lake_directory)

f <- read.csv(file.path(config_obs$file_path$targets_directory, config_obs$site_id, "UC_analysis_2021", paste0(config_obs$site_id,"-targets-insitu.csv")))
# limit to first full month when sensors were out in the first year of this study (2019)
f <- f %>% 
  filter(time > as.POSIXct('2018-05-01 00:00:00') & time < as.POSIXct('2018-06-01 00:00:00'))
#NOTE: this is generally pretty consistent across years so will stick with this initialization
##################################################
lag <- f %>% 
  group_by(variable, depth) %>% 
  mutate(lag1 = dplyr::lag(observed, n = 1L))

lag_oxy <- lag %>% 
  filter(variable=='oxygen') %>% 
  mutate(resid = observed- lag1)
oxy_sd <- sd(lag_oxy$resid, na.rm = TRUE) 
oxy_sd

lag_temp <- lag %>% 
  filter(variable=='temperature') %>% 
  mutate(resid = observed- lag1)
temp_sd <- sd(lag_temp$resid, na.rm = TRUE) 
temp_sd

st_config <- read.csv(file.path(lake_directory, 'configuration/UC_analysis/states_config.csv'))

# update initial model_sd
st_config$model_sd[st_config$state_names=='temp'] <- temp_sd
st_config$model_sd[st_config$state_names=='OXY_oxy'] <- oxy_sd

write.csv(st_config, file.path(lake_directory, 'configuration/UC_analysis/states_config.csv'),
          row.names = FALSE, quote = FALSE) 

# now update depth_model_sd for discrete depths where temperature obs are available
temp_depths <- lag_temp %>% 
  group_by(depth) %>% 
  mutate(sd_resid = sd(resid, na.rm = TRUE)) %>% 
  distinct(depth, .keep_all = TRUE) %>% 
  select(depth, sd_resid)

temp_depths <- temp_depths[order(temp_depths$depth),]
temp_depths

depth_temp_sd <- temp_depths %>% 
  rename(temp = sd_resid)

depth_model_sd <- read.csv(file.path(lake_directory, 'configuration/UC_analysis/depth_model_sd.csv'))
depth_model_sd <- NA
depth_model_sd <- depth_temp_sd
write.csv(depth_temp_sd,
          file.path(lake_directory, 'configuration/UC_analysis/depth_model_sd.csv'),
          row.names = FALSE, quote = FALSE)
