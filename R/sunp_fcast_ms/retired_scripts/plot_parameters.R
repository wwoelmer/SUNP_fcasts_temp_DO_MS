# plot parameter evolution for each year

library(lubridate)
library(tidyverse)
library(ggpubr)
library(arrow)

lake_directory <- here::here()

# rerun the scoring to save parameters
# some subsetting variables
folders <- c('all_UC')
site_id <- 'sunp'
dates <- c("2021-07-12 00:00:00")

buoy_dates <- c(seq.Date(as.Date('2021-08-04'), as.Date('2021-10-17'), by = 'day'),
                seq.Date(as.Date('2022-08-04'), as.Date('2022-10-17'), by = 'day'))
buoy_dates <- paste0(buoy_dates, " 00:00:00")


buoy_dates <- c(seq.Date(as.Date('2021-07-12'), as.Date('2021-08-04'), by = 'day'),
                seq.Date(as.Date('2022-04-28'), as.Date('2022-08-04'), by = 'day'))
buoy_dates <- paste0(buoy_dates, " 00:00:00")

targets_file = file.path(lake_directory, "targets/sunp/UC_analysis_2022/sunp-targets-insitu.csv")
forecast_df = sc   
use_s3 = FALSE
bucket = NULL
endpoint = NULL
local_directory = file.path(lake_directory, "scores/sunp_parameters")
variable_types = c("state", "parameter")
fcast_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"forecasts/parquet"))

for(i in 1:length(buoy_dates)){
  sc <- arrow::open_dataset(fcast_dir) |> 
    filter(
      model_id %in% folders,
      reference_datetime==buoy_dates[i]) %>% 
    collect() 
  
  generate_forecast_score_arrow(
    targets_file = targets_file,
    forecast_df = sc,
    use_s3 = use_s3,
    bucket = bucket,
    endpoint = endpoint,
    local_directory = local_directory,
    variable_types = c("state", "parameter")
  )
  
}

#######################################################################################
# read in the files
parms <- c("Fsed_oxy_zone1", "Fsed_oxy_zone2", "Fsed_oxy_zone3",
           "lw_factor", "zone1temp","zone2temp", "zone3temp")

score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp_parameters"))

p <- arrow::open_dataset(score_dir) |> 
  filter(variable %in% parms) %>% 
  collect() 

p <- p %>% 
  mutate(doy = yday(datetime),
         year = year(datetime))

p$variable <- 
  factor(p$variable, 
       levels = c("Fsed_oxy_zone1",
                  "Fsed_oxy_zone2",
                  "Fsed_oxy_zone3",
                  "zone1temp",
                  "zone2temp",
                  "zone3temp",
                  "lw_factor")) 

p %>% 
  filter(horizon==1) %>% 
ggplot(aes(x = doy, y = mean, color = as.factor(year))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = as.factor(year)), alpha = 0.3) +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values =  c('#17BEBB', '#9E2B25')) +
  facet_wrap(~variable, scales = 'free') +
  theme_bw()

p %>% 
  filter(horizon==1,
         variable %in% c("Fsed_oxy_zone1",
                         "Fsed_oxy_zone2",
                         "Fsed_oxy_zone3")) %>% 
  ggplot(aes(x = doy, y = mean, color = as.factor(variable))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = as.factor(variable)), alpha = 0.3) +
  facet_wrap(~year) +
  theme_bw() +
  ggtitle('Fsed at 3 zones')

p %>% 
  filter(horizon==1,
         variable %in% c("tempsed_18_33",
                         "tempsed_10_18",
                         "tempsed_0_10")) %>% 
  ggplot(aes(x = doy, y = mean, color = as.factor(variable))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = as.factor(variable)), alpha = 0.3) +
  facet_wrap(~year) +
  theme_bw() +
  ggtitle('Sed Temp at 3 zones')

p %>% 
  filter(horizon==1,
         variable %in% c("lw_factor")) %>% 
  ggplot(aes(x = doy, y = mean, color = as.factor(variable))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = as.factor(variable)), alpha = 0.3) +
  facet_wrap(~year) +
  theme_bw() +
  ggtitle('LongWave Factor')



#######################################################################################
# rerun scoring to 



