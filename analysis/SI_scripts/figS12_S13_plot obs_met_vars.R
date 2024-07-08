# look at observed weather over study period

library(tidyverse)
library(ggpubr)

lake_directory <- here::here()

# download stacked met from bucket or add to zenodo for initial download?
#FLAREr::get_stacked_noaa(lake_directory, config, averaged = TRUE)
#file was downloaded in `02_download_files.R`

dat <- ncdf4::nc_open(file.path(lake_directory, "drivers/noaa/NOAAGEFS_1hr_stacked_average/sunp/observed-met-noaa_sunp.nc"))
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

buoy_dates <- c(seq.Date(as.Date('2021-08-04'), as.Date('2021-10-17'), by = 'day'),
                seq.Date(as.Date('2022-08-04'), as.Date('2022-10-17'), by = 'day'))
buoy_dates <- format(buoy_dates, "%m-%d")

met <- met %>% 
  distinct(time, .keep_all = TRUE) %>% 
  mutate(year = year(time),
         mo_day = format(as.Date(time), "%m-%d")) %>% 
  filter(year %in% c('2021', '2022'),
         mo_day %in% buoy_dates) %>% 
  mutate(hours = format(as.POSIXct(time),format = "%H:%M:%S"),
         mo_day_time = paste0(mo_day, " ", hours))

met_long <- met %>% pivot_longer(cols = glm_met_vars, names_to = 'variable', values_to = 'value')



###################################################################################
## now plot variability figures
a <- ggplot(met_long, aes(x = as.factor(year), fill = as.factor(year), y = value)) +
  geom_boxplot() +
  labs(fill = 'Year') +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~variable, scales = 'free') +
  theme_bw() +
  xlab('Year')

b <- ggplot(met_long, aes(x = value, fill = as.factor(year))) +
  geom_density(alpha = 0.8) +
  labs(fill = 'Year') +  
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~variable, scales = 'free') +
  theme_bw()

fig_s9 <- ggarrange(a, b, common.legend = TRUE)
ggsave('./figures/figS12.png', fig, scale = 0.5, dpi = 300, unit = "mm", width = 625, height = 220)

#####
mix_21 <- "10-04 00:00:00"
mix_22 <- "09-23 00:00:00"
fig_s10 <-
  ggplot(met_long, aes(x = as.POSIXct(mo_day_time, format = "%m-%d %H:%M:%S"), y = value, color = as.factor(year))) +
  geom_line() +
  scale_x_datetime(date_labels = "%b") +
  #xlim(as.Date('08-04', format = "%m-%d"), as.Date('08-20', format = "%m-%d")) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~variable, scales = 'free') +
  theme_bw() +
  geom_vline(aes(xintercept = as.POSIXct(mix_21, format = "%m-%d %H:%M:%S"), linetype = '2021')) +
  geom_vline(aes(xintercept = as.POSIXct(mix_22, format = "%m-%d %H:%M:%S"), linetype = '2022')) +
  labs(color = 'Year') +
  xlab("Date")

ggsave('./figures/figs13.png', fig_s10, scale = 0.5, dpi = 300, unit = "mm", width = 500, height = 220)

  