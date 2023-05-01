library(lubridate)
library(tidyverse)
library(ggpubr)
library(arrow)
library(scales)
library(rMR)
library(scoringRules)
library(Metrics)
#install.packages('RcppRoll')
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


### select horizons for all metrics
ggplotly(sc %>% 
  filter(horizon %in% c(1, 7, 14, 21, 35)) %>% 
ggplot(aes(x = doy, y = crps, color = as.factor(horizon), linetype = as.factor(year))) +
  geom_line() +
  facet_grid(depth~variable))

sc %>% 
  filter(horizon %in% c(1, 7, 14, 21, 35)) %>% 
  ggplot(aes(x = doy, y = logs, color = as.factor(horizon), linetype = as.factor(year))) +
  geom_line() +
  facet_grid(depth~variable)

#####################
# plot crps with pattern of observations from each year
sc %>% 
  filter(horizon %in% c(1)) %>% 
  ggplot(aes(x = doy, y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'horizon', linetype = 'year') +
  facet_grid(depth~variable, scales = 'free') +
  geom_line(aes(y = observation/5, color = as.factor(year), linetype = 'obs'))+
  scale_y_continuous(name = 'CRPS', sec.axis = sec_axis(trans = ~.*5, name = 'observations'))
  

sc %>% 
  filter(horizon %in% c(7),
         doy > 221) %>% 
  ggplot(aes(x = doy, y = crps, color = as.factor(horizon), linetype = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('blue', 'black')) +
  labs(color = 'horizon', linetype = 'year') +
  facet_grid(depth~variable) +
  geom_line(aes(y = observation, color = 'black', linetype = as.factor(year))) 


sc %>% 
  filter(horizon %in% c(21),
         doy > 250-14) %>% 
  ggplot(aes(x = doy, y = rmse, color = as.factor(horizon), linetype = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('maroon', 'black')) +
  labs(color = 'horizon', linetype = 'year') +
  facet_grid(depth~variable) +
  geom_line(aes(y = observation, color = 'black', linetype = as.factor(year))) 
#  scale_y_continuous(name = 'RMSE', sec.axis = sec_axis(trans = ~.*5, name = 'observations'))

sc %>% 
  filter(horizon %in% c(35),
         doy > 250) %>% 
  ggplot(aes(x = doy, y = rmse, color = as.factor(horizon), linetype = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('purple', 'black')) +
  labs(color = 'horizon', linetype = 'year') +
  facet_grid(depth~variable) +
  geom_line(aes(y = observation, color = 'black', linetype = as.factor(year))) 

###
diff_yr <- sc %>% 
  ungroup() %>% 
  select(horizon:variable, observation, year, doy, crps, logs, rmse) %>% 
  distinct(horizon, depth, variable, year, doy, .keep_all = TRUE) %>% 
  mutate(year = factor(year))
diff_yr_wide <- diff_yr %>% 
  pivot_wider(names_from = year, values_from = c(crps, logs, rmse, observation))

diff_yr_wide %>% 
  filter(horizon %in% c(1, 7, 14, 21, 35)) %>% 
  ggplot(aes(x = crps_2021, y = crps_2022, color = as.factor(horizon))) +
  geom_line() +
  geom_abline(slope = 1) +
  facet_grid(depth~variable, scales = 'free')

diff_yr_wide %>% 
  filter(horizon %in% c(1, 7, 14, 21, 35)) %>% 
  ggplot(aes(x = doy, y = crps_2021 - crps_2022, color = as.factor(horizon))) +
  geom_line() +
  facet_grid(depth~variable) +
  geom_hline(yintercept = 0)

diff_yr_wide %>% 
  filter(horizon %in% c(1, 7, 14, 21, 35)) %>% 
  ggplot(aes(x = doy, y = logs_2021 - logs_2022, color = as.factor(horizon))) +
  geom_line() +
  facet_grid(depth~variable, scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = doy, y = observation_2021 - observation_2022, color = 'black'))

diff_yr_wide %>% 
  filter(horizon %in% c(1)) %>% 
  ggplot(aes(x = doy, y = logs_2021 - logs_2022, color = 'log')) +
  geom_line() +
  facet_grid(depth~variable, scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = doy, y = observation_2021 - observation_2022, color = 'obs')) +
  scale_color_manual(values = c('orange', 'black'))

diff_yr_wide %>% 
  filter(horizon %in% c(7)) %>% 
  ggplot(aes(x = doy, y = logs_2021 - logs_2022, color = 'log')) +
  geom_line() +
  facet_grid(depth~variable, scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = doy, y = observation_2021 - observation_2022, color = 'obs')) +
  scale_color_manual(values = c('orange', 'black'))

diff_yr_wide %>% 
  filter(horizon %in% c(35)) %>% 
  ggplot(aes(x = doy, y = logs_2021 - logs_2022, color = 'log')) +
  geom_line() +
  facet_grid(depth~variable, scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = doy, y = observation_2021 - observation_2022, color = 'obs')) +
  scale_color_manual(values = c('orange', 'black'))

#####
# CRPS
diff_yr_wide %>% 
  filter(horizon %in% c(1)) %>% 
  ggplot(aes(x = doy, y = logs_2021 - logs_2022, color = 'log')) +
  geom_line() +
  facet_grid(depth~variable, scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = doy, y = observation_2021 - observation_2022, color = 'obs')) +
  scale_color_manual(values = c('orange', 'black'))

diff_yr_wide %>% 
  filter(horizon %in% c(7)) %>% 
  ggplot(aes(x = doy, y = logs_2021 - logs_2022, color = 'log')) +
  geom_line() +
  facet_grid(depth~variable, scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = doy, y = observation_2021 - observation_2022, color = 'obs')) +
  scale_color_manual(values = c('orange', 'black'))

diff_yr_wide %>% 
  filter(horizon %in% c(35)) %>% 
  ggplot(aes(x = doy, y = crps_2021 - crps_2022, color = 'crps')) +
  geom_line() +
  facet_grid(depth~variable, scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = doy, y = observation_2021 - observation_2022, color = 'obs')) +
  scale_color_manual(values = c('orange', 'black'))

##################################################
### running CV of observations
cv <- na.omit(diff_yr)
roll_cv <- cv %>% 
  group_by(horizon, depth, variable, year) %>% 
  mutate(roll_mean_obs = rollmean(observation, k = 7, fill = NA),
         roll_sd_obs = rollapply(observation, width = 7, FUN = sd, fill = 0),
         roll_cv_obs = roll_sd_obs/roll_mean_obs,
         roll_mean_crps = rollmean(crps, k = 7, fill = NA),
         roll_sd_crps = rollapply(crps, width = 7, FUN = sd, fill = 0),
         roll_cv_crps = roll_sd_crps/roll_mean_crps)

roll_cv %>% 
  filter(horizon == 1) %>% 
ggplot(aes(x = doy, y = roll_cv_obs, color = as.factor(year))) +
  geom_line() +
  facet_grid(depth~variable, scales = 'free') 
  
roll_cv %>% 
  filter(horizon == 1) %>% 
  ggplot(aes(x = doy, y = roll_cv_crps, color = as.factor(year))) +
  facet_grid(depth~variable, scales = 'free') 

t <- roll_cv %>% 
  filter(horizon==1,
         variable=='temperature (C)') %>% 
  ggplot(aes(x = roll_cv_obs, y = roll_cv_crps, color = as.factor(year))) +
  geom_point() +
  geom_smooth() +
  ggtitle('temperature') +
  facet_wrap(~depth, scales = 'free') +
  labs(color = 'year')

o <- roll_cv %>% 
  filter(horizon==1,
         variable=='oxygen (mg/L)') %>% 
  ggplot(aes(x = roll_cv_obs, y = roll_cv_crps, color = as.factor(year))) +
  geom_point() +
  geom_smooth() +
  ggtitle('oxygen') +
  facet_wrap(~depth, scales = 'free') +
  labs(color = 'year')

ggarrange(t, o, common.legend = TRUE)
