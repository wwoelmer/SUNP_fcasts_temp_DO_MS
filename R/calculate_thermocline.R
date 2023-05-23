library(lubridate)
library(tidyverse)
library(rLakeAnalyzer)

lake_directory <- here::here()

dat <- read.csv(file.path(lake_directory, "targets/sunp/UC_analysis_2022/sunp-targets-insitu.csv"))
dat <- dat %>% 
  mutate(year = year(time)) %>% 
  filter(variable=='temperature',
         year %in% c(2021, 2022))

ts <- ggplot(dat, aes(x = as.Date(time), y = observed, color = as.factor(depth))) +
  geom_line() +
  facet_wrap(~year, scales = 'free_x')

dat <- dat %>% 
  group_by(time, year) %>% 
  mutate(therm = thermo.depth(observed, depth, seasonal = TRUE))

td <- ggplot(dat, aes(x = as.Date(time), y = therm)) +
  geom_line() +
  facet_wrap(~year, scales = 'free_x')


library(patchwork)
ts + td
