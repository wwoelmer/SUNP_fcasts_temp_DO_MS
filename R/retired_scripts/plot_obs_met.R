library(tidyverse)
setwd(here::here())
#####################################################################################################################
# plot weather variables across years
# mean and variance of rainfall, temp, wind

# noaa stacked met 
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


obs_met_nc <- ncdf4::nc_open('./drivers/noaa/NOAAGEFS_1hr_stacked_average/sunp/observed-met-noaa_sunp.nc')

obs_met_time <- ncdf4::ncvar_get(obs_met_nc, "time")

origin <- stringr::str_sub(ncdf4::ncatt_get(obs_met_nc, "time")$units, 13, 28)

origin <- lubridate::ymd_hm(origin)

obs_met_time <- origin + lubridate::hours(obs_met_time)

met <- tibble::tibble(time = obs_met_time)

for(i in 1:length(cf_met_vars)){
  
  met <- cbind(met, ncdf4::ncvar_get(obs_met_nc, cf_met_vars[i]))
}

ncdf4::nc_close(obs_met_nc)

names(met) <- c("time", glm_met_vars)

met <- met %>% 
  mutate(year = year(time),
         mo_day = format(as.Date(time), "%m-%d")) %>% 
  filter(mo_day > '07-09' & mo_day < '10-31',
         year > 2020)


met_long <- met %>% 
  pivot_longer(AirTemp:Rain, names_to = 'variable', values_to = 'value')

ggplot(met_long, aes(x = as.factor(year), y = value)) +
  geom_boxplot(aes(group = year, fill = as.factor(year))) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  xlab('year') +
  labs(fill = 'Year') +
  facet_wrap(~variable, scales = 'free_y')

ggplot(met_long, aes(x = as.Date(mo_day, format = "%m-%d"), y = value, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~variable, scales = 'free_y')

ggplot(met_long, aes(value, fill = as.factor(year), group = as.factor(year))) +
  geom_histogram(position = 'identity', alpha = 0.6) +
  facet_wrap(~variable, scales = 'free')


met_mean <- met %>% 
  group_by(year) %>% 
  summarise_at(c("AirTemp", "ShortWave", "LongWave", "RelHum", "WindSpeed", "Rain"), mean, na.rm = TRUE)

met_sd <- met %>% 
  group_by(year) %>% 
  summarise_at(c("AirTemp", "ShortWave", "LongWave", "RelHum", "WindSpeed", "Rain"), sd, na.rm = TRUE)

met_min <- met %>% 
  group_by(year) %>% 
  summarise_at(c("AirTemp", "ShortWave", "LongWave", "RelHum", "WindSpeed", "Rain"), min, na.rm = TRUE)

met_max <- met %>% 
  group_by(year) %>% 
  summarise_at(c("AirTemp", "ShortWave", "LongWave", "RelHum", "WindSpeed", "Rain"), max, na.rm = TRUE)

rain_sum <- met %>% 
  group_by(year) %>% 
  mutate(rain_sum = sum(Rain)) %>% 
  distinct(rain_sum, .keep_all = TRUE)

ggplot(rain_sum, aes(x = year, y = rain_sum)) +
  geom_point(size = 4)

# buoy met data
download.file('https://github.com/FLARE-forecast/SUNP-data/raw/sunp-buoy-data/SUNP_buoy_met.csv', './buoy_met.csv')
met2 <- read.csv('./buoy_met.csv')
d_head<-read.csv('./buoy_met.csv', skip=1, as.is=T) #get header minus wonky Campbell rows
d <-read.csv('./buoy_met.csv', skip=4, header=F) #get data minus wonky Campbell rows
names(d)<-names(d_head) #combine the names to deal with Campbell logger formatting



# check why DO doesn't start until Aug at 10m in 2020
