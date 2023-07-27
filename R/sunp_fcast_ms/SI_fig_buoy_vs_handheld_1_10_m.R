# script to make figure comparing buoy observations to hand-held YSI measurements on the same day

###############################################################################
# read in buoy obs
library(tidyverse)
library(lubridate)
library(ggpubr)

setwd(here::here())

tgts <- read.csv('./targets/sunp/SUNP_fsed_deep_DA/sunp-targets-insitu.csv')
years <- c('2021', '2022')
depths <- c(1.0, 10.0)

obs <- tgts %>% 
  mutate(year = year(time)) %>% 
  filter(year %in% years,
         depth %in% depths)

obs <- obs %>% 
  select(time, depth, observed, variable) %>% 
  filter(variable=='oxygen',
         observed > 0) 

# limit to the days that we will analyze for forecasts (i.e., when we have NOAA forecasts for)
buoy_dates <- c(seq.Date(as.Date('2021-08-04'), as.Date('2021-10-17'), by = 'day'),
                seq.Date(as.Date('2022-08-04'), as.Date('2022-10-17'), by = 'day'))

obs <- obs %>% 
  filter(as.Date(time) %in% buoy_dates) %>% 
  mutate(year = year(time),
         mo_day = format(as.Date(time), "%m-%d")) %>% 
  filter(mo_day > "06-29" & mo_day < '10-18')

obs_mgL <- obs[obs$variable=='oxygen',]

#####################################################################################
# read in handheld DO measurements
library(gsheet)
library(lubridate)

link <- "https://docs.google.com/spreadsheets/d/1IfVUlxOjG85S55vhmrorzF5FQfpmCN2MROA_ttEEiws/edit#gid=1721211942"
ysi <- gsheet::gsheet2tbl(link, sheetid = "ManualDO")
ysi$Date <- as.Date(ysi$Date)
ysi$year <- year(ysi$Date)
ysi$mo_day <- format(as.Date(ysi$Date), "%m-%d")
ysi$depth <- round(ysi$Depth)

ysi_sub <- ysi %>% 
  filter(depth %in% c(1, 10),
         Site=='buoy',
         year %in% c(2021, 2022),
         mo_day > "08-01")  


########## make plot with both observations
ggplot(data = obs_mgL, aes(x = as.Date(mo_day, format = "%m-%d"), y = observed*32/1000, color = as.factor(year))) +
  geom_line() +
  geom_point(data = ysi_sub, aes(x = as.Date(mo_day, format = "%m-%d"), y = DO_mgL, color = as.factor(year))) +
  facet_wrap(~depth, ncol = 1) +
  scale_x_date(date_labels = "%b") +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  ylab('DO (mg/L)') +
  xlab('Date') +
  labs(color = 'Year') +
  theme_bw()

  
