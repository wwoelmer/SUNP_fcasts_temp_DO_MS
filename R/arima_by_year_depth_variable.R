# compare time series, look at autocorrelation

library(tidyverse)
library(lubridate)
library(ggpubr)

setwd(here::here())

tgts <- read.csv('./targets/sunp/UC_analysis_2022/sunp-targets-insitu.csv')
years <- c('2021', '2022')
oxy <- tgts %>% 
  mutate(year = year(time)) %>% 
  filter(year %in% years)

oxy <- oxy %>% 
  select(time, depth, observed, variable) %>% 
  filter(variable=='oxygen' &
         observed > 0) 

# limit to the days that we will analyze for forecasts (i.e., when we have NOAA forecasts for)
buoy_dates <- c(seq.Date(as.Date('2021-08-04'), as.Date('2021-10-17'), by = 'day'),
                seq.Date(as.Date('2022-08-04'), as.Date('2022-10-17'), by = 'day'))

oxy <- oxy %>% 
  filter(as.Date(time) %in% buoy_dates) %>% 
  mutate(year = year(time),
         mo_day = format(as.Date(time), "%m-%d")) %>% 
  filter(mo_day > "06-29" & mo_day < '10-18')


temp <- read.csv('./targets/sunp/UC_analysis_2022/sunp-targets-insitu.csv')
temp <- temp %>% 
  select(time, depth, observed, variable) %>% 
  filter(variable=='temperature') %>% 
  filter(as.Date(time) %in% buoy_dates) %>% 
  mutate(year = year(time),
         mo_day = format(as.Date(time), "%m-%d")) %>% 
  filter(year %in% years,
         as.Date(time) > as.Date('2021-06-08'))

# round 1.5 data to 1.0m
temp <- temp %>% 
  mutate(depth_cor = ifelse(depth==1.5, 1.0, depth)) %>% 
  mutate(depth = depth_cor) %>% 
  select(-depth_cor)

obs <- full_join(obs, temp) %>% 
  filter(depth %in% c(1.0, 10.0))
##################################################################################################

## ddply across depth, variable, and year

library(tidyr)

acf(obs$observed, )

out <- plyr::ddply(obs, c("depth", "year", "variable"), \(x) {
  p <- pacf(x$observed, lag.max = 30)
  plot(p, main = paste0(x$year[1], " ", x$variable[1], " ", x$depth[1]))
})

out.ar <- plyr::ddply(obs, c("depth", "year", "variable"), \(x) {
  print(paste0(x$year[1], "", x$variable[1], " ", x$depth[1]))
  print(auto.arima(x$observed))
})

out.ar <- plyr::ddply(obs, c("depth", "year", "variable"), \(x) {
  print(paste0(x$year[1], "", x$variable[1], " ", x$depth[1]))
  print(arima(x$observed, order = c(1, 1, 0)))
})

tst <- obs %>% 
  filter(year==2022,
         depth==10,
         variable=="oxygen")

ar <- arima(tst$observed, order = c(1, 1, 0))
ar$coef
ar$fitted
ar
ar$model
ar$residuals
