library(lubridate)
library(tidyverse)
library(rLakeAnalyzer)
library(patchwork)

lake_directory <- here::here()
sim_name <- 'SUNP_fcasts_temp_DO'

dat <- read.csv(file.path(lake_directory, paste0("targets/sunp/", sim_name, "/sunp-targets-insitu.csv")))

dat <- dat %>% 
  mutate(year = year(time)) %>% 
  filter(variable=='temperature',
         year %in% c(2021, 2022)) 

unique(dat$depth)

dat <- dat %>% 
  mutate(depth_rnd = ifelse(depth > 0.2, round(depth), depth)) %>% 
  distinct(time, depth_rnd, variable, .keep_all = TRUE)

unique(dat$depth_rnd)

ts <- ggplot(dat[dat$depth < 10.5,], aes(x = as.Date(time), y = observed, color = as.factor(depth_rnd))) +
  geom_line() +
  geom_point(data = dat[dat$depth > 10.5,], aes(x = as.Date(time), y = observed, color = as.factor(depth_rnd))) +
  facet_wrap(~year, scales = 'free_x') +
  xlab('Date') +
  ylab('Temperature (°C)') +
  labs(color = 'Depth (m)') +
  theme_bw()
ts

dat <- dat %>% 
  group_by(time, year) %>% 
  mutate(therm = thermo.depth(observed, depth_rnd, seasonal = TRUE))

td <- ggplot(dat, aes(x = as.Date(time), y = therm)) +
  geom_line() +
  facet_wrap(~year, scales = 'free_x') +
  xlab('Date') +
  ylab('Thermocline Depth (m)') +
  theme_bw()
td

tfig <- ggarrange(ts, td, labels = 'auto')
ggsave('./figures/thermo_depth.tiff', tfig, scale = 0.5, dpi = 300, unit = "mm", width = 625, height = 220)
