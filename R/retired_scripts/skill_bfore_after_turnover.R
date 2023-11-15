# look at variability in temp and DO forecast skill before and after turnover each year
library(lubridate)
library(tidyverse)
library(ggpubr)
library(arrow)
library(scales)
library(rMR)
library(scoringRules)
library(Metrics)
library(RcppRoll)

lake_directory <- here::here()

# some subsetting variables
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)
horizons <- c(1:35)
folders <- c('all_UC')

########################################################################
# read in the scores and calculate variance
score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp/UC_analysis_2021/start_06_30", folders[1]))

sc <- arrow::open_dataset(score_dir) |> 
  filter(variable %in% vars,
         depth %in% depths) %>% 
  collect() 

for(i in 1:length(folders)){
  score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp/UC_analysis_2021/start_06_30", folders[i]))
  
  temp <- arrow::open_dataset(score_dir) |> 
    filter(variable %in% vars,
           depth %in% depths) %>% 
    collect() 
  
  sc <- rbind(sc, temp)
  
}

# now read in 2022 data
for(i in 1:length(folders)){
  score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp/UC_analysis_2022", folders[i]))
  
  temp <- arrow::open_dataset(score_dir) |> 
    filter(variable %in% vars,
           depth %in% depths) %>% 
    collect() 
  
  sc <- rbind(sc, temp)
  
}

# vector of dates when obs are available (before buoy is taken into harbor)
##### make these the same dates for each year for equal comparison
buoy_dates <- c(seq.Date(as.Date('2021-08-04'), as.Date('2021-10-17'), by = 'day'),
                seq.Date(as.Date('2022-08-04'), as.Date('2022-10-17'), by = 'day'))

sc <- sc %>% 
  mutate(doy = yday(datetime),
         year = year(datetime)) %>% 
  select(-c(family, site_id)) %>% 
  filter(horizon > 0,#) %>% 
         as.Date(datetime) %in% buoy_dates) %>% 
  select(model_id, reference_datetime, datetime, horizon, depth, variable, everything()) 

# convert oxy crps and obs to mg/L
sc <- sc %>% 
  mutate(crps = ifelse(variable=='temperature', crps, (crps*32/1000)),
         observation = ifelse(variable=='temperature', observation, (observation*32/1000)),
         mean = ifelse(variable=='temperature', mean, (mean*32/1000)),
         sd = ifelse(variable=='temperature', sd, (sd*32/1000)))

sc$variable <- factor(sc$variable, levels = c('temperature', 'oxygen'), 
                      ordered = TRUE, labels = c('temperature (C)', 'oxygen (mg/L)'))

### calculate rmse
sc <- sc %>% 
  group_by(depth, variable, horizon, datetime) %>% 
  mutate(rmse = rmse(observation, mean)) 

# add month day
sc <- sc %>% 
  mutate(mo_day = format(datetime, "%m-%d"))

## read in mixing dates file
mix <- read.csv('./mixing_dates.csv')
dates <- unique(as.Date(mix$time))
date_range <- c(seq.Date(dates[1] - 14, dates[1] + 14, by = 'day'),
                     seq.Date(dates[2] - 14, dates[2] + 14, by = 'day'))
date_range

mixcast <- sc %>% 
  filter(as.Date(datetime) %in% date_range)
mixcast <- mixcast %>% 
  mutate(mix_day = ifelse(year==2021, '2021-10-04', '2022-09-23'))

mixcast$mix_day <- as.Date(mixcast$mix_day)

mixcast <- mixcast %>% 
  mutate(timing = ifelse(datetime < mix_day, 'before', 'after'),
         timing = factor(timing, levels = c('before', 'after'))) %>% 
  group_by(year, variable, depth, horizon, timing) %>% 
  mutate(cv_obs = sd(observation)/mean(observation),
         cv_crps = sd(crps)/mean(crps))

mixcast %>% 
  filter(horizon==1) %>% 
ggplot(aes(x = timing, y = crps, color = as.factor(year))) +
  geom_boxplot() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(depth~variable, scales = 'free') +
  ggtitle('Horizon = 1')

mixcast %>% 
  filter(horizon==1) %>% 
  ggplot(aes(x = timing, y = observation, color = as.factor(year))) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  geom_boxplot() +
  facet_wrap(depth~variable, scales = 'free') +
  ggtitle('Observations')


mixcast %>% 
  filter(horizon==35) %>% 
  ggplot(aes(x = timing, y = cv_crps, color = as.factor(year))) +
  geom_point() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(depth~variable, scales = 'free') +
  ggtitle('Horizon = 35')

mixcast %>% 
  filter(horizon==1) %>% 
  ggplot(aes(x = timing, y = cv_obs, color = as.factor(year))) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  geom_point() +
  facet_wrap(depth~variable, scales = 'free') +
  ggtitle('Observations')
