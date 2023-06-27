library(lubridate)
library(tidyverse)
library(rLakeAnalyzer)
library(patchwork)

lake_directory <- here::here()

dat <- read.csv(file.path(lake_directory, "targets/sunp/SUNP_fsed_deep_DA/sunp-targets-insitu.csv"))

dat <- dat %>% 
  mutate(year = year(time)) %>% 
  filter(variable=='temperature',
         year %in% c(2021, 2022),
         depth < 11) 

unique(dat$depth)

ts <- ggplot(dat, aes(x = as.Date(time), y = observed, color = as.factor(depth))) +
  geom_line() +
  facet_wrap(~year, scales = 'free_x') +
  xlab('Date') +
  ylab('Temperature (°C)') +
  labs(color = 'Depth (m)') +
  theme_bw()

dat <- dat %>% 
  group_by(time, year) %>% 
  mutate(therm = thermo.depth(observed, depth, seasonal = TRUE))

td <- ggplot(dat, aes(x = as.Date(time), y = therm)) +
  geom_line() +
  facet_wrap(~year, scales = 'free_x') +
  xlab('Date') +
  ylab('Thermocline Depth (m)') +
  theme_bw()


ggarrange(ts, td, labels = 'auto')
