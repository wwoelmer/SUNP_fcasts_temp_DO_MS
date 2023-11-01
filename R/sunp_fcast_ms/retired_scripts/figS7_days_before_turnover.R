## plot skill metrics over doy

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
years <- c('2021', '2022')
depths <- c(1.0, 10.0)
horizons <- c(1:35)
sim_name <- 'SUNP_fcasts_temp_DO'

# vector of dates when obs are available (before buoy is taken into harbor)
buoy_dates <- c(seq.Date(as.Date('2021-08-04'), as.Date('2021-10-17'), by = 'day'),
                seq.Date(as.Date('2022-08-04'), as.Date('2022-10-17'), by = 'day'))

################################################3
# calculate date of turnover
temp <- read.csv(paste0('./targets/sunp/', sim_name, '/sunp-targets-insitu.csv'))
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

## limit to the same time duration between years
temp <- temp %>% 
  filter(as.Date(time) %in% buoy_dates)

temp <- temp %>% 
  distinct()

to <- temp %>% 
  filter(depth==1 | depth==10 | depth==1.5) %>%
  pivot_wider(names_from = "depth", names_prefix = "depth", values_from = "observed") %>% 
  mutate(depth_surf = ifelse(is.na(depth1), depth1.5, depth1)) %>% 
  group_by(year, mo_day) %>% 
  mutate(t_diff = depth_surf - depth10) %>% 
  mutate(mix = ifelse(abs(t_diff) < 1, 1, 0))

to <- to %>% 
  select(year, mo_day, t_diff, mix, depth_surf, depth10) #%>% 

t_dates <- to %>% 
  filter(mo_day > '08-10') %>% 
  select(year:depth10) %>% 
  group_by(year, mix) %>% 
  slice(1) %>% 
  filter(mix > 0) 


# 2019-09-14
# 2020-09-07
# 2021-10-04
# 2022-09-23


t_dates <- t_dates %>% 
  rename(mix_day = mo_day,
         temperature_1 = depth_surf,
         temperature_10 = depth10) %>% 
  mutate(time = as.Date(paste0(year, "-", mix_day)))

t_dates <- t_dates %>% 
  ungroup() %>% 
  pivot_longer(cols = c(temperature_1, temperature_10), names_to = 'variable', values_to = 'observed') %>% 
  separate(variable, into = c('variable', 'depth')) %>% 
  mutate(depth = as.numeric(depth)) %>% 
  select(-c(t_diff, mix))

# again for oxygen
tgts <- read.csv(paste0('./targets/sunp/', sim_name, '/sunp-targets-insitu.csv'))

obs <- tgts %>% 
  mutate(year = year(time)) %>% 
  filter(year %in% years,
         depth %in% depths)

obs <- obs %>% 
  select(time, depth, observed, variable) %>% 
  filter(variable=='oxygen',
         observed > 0) 

obs <- obs %>% 
  filter(as.Date(time) %in% buoy_dates) %>% 
  mutate(year = year(time),
         mo_day = format(as.Date(time), "%m-%d")) %>% 
  filter(mo_day > "06-29" & mo_day < '10-18')

obs_mgL <- obs[obs$variable=='oxygen',]

o_dates <- obs_mgL %>% 
  filter(as.Date(time) %in% t_dates$time) %>% 
  mutate(time = as.Date(time)) %>% 
  rename(mix_day = mo_day)

# make data frame of oxy and mixing dates
obs_mgL$time <- as.Date(obs_mgL$time)
mix_oxy <- full_join(obs_mgL, t_dates)

mix_dates <- full_join(o_dates, t_dates)

###########################################################################
# read in forecasts
score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp", sim_name))

sc <- arrow::open_dataset(score_dir) |> 
  filter(variable %in% vars,
         depth %in% depths) %>% 
  collect() 

sc <- sc %>% 
  mutate(doy = yday(datetime),
         year = year(datetime)) %>% 
  select(-c(family, site_id)) %>% 
  filter(horizon > 0,#) %>% 
         as.Date(reference_datetime) %in% buoy_dates,
         doy < 291) %>%
  select(model_id, reference_datetime, datetime, horizon, depth, variable, everything()) 

# convert oxy crps and obs to mg/L
sc <- sc %>% 
  mutate(crps = ifelse(variable=='temperature', crps, (crps*32/1000)),
         observation = ifelse(variable=='temperature', observation, (observation*32/1000)),
         mean = ifelse(variable=='temperature', mean, (mean*32/1000)),
         sd = ifelse(variable=='temperature', sd, (sd*32/1000)))

# add month day
sc <- sc %>% 
  mutate(mo_day = format(datetime, "%m-%d"))





mix_dates$time <- as.Date(mix_dates$time)
mix_dates$depth <- as.numeric(mix_dates$depth)
mix_dates <- mix_dates %>% 
  select(-observed, -mix_day) %>% 
  rename(mix_day = time)

scjoin <- sc %>% 
  select(datetime, variable, depth, crps, horizon) %>% 
  mutate(time = as.Date(datetime),
         year = year(datetime)) %>% 
  ungroup() %>% 
  select(time, depth, year, horizon, variable, crps)

df <- left_join(scjoin, mix_dates, by = c("variable", "depth", "year"))

#####################################################################################
### calculate days before turnover to set x-axis
df <- df %>% 
  mutate(dbt = time -mix_day)

#####################################################################################
# plot select horizons

o1 <- df %>% 
  filter(horizon==1,
         variable=="oxygen") %>% 
  ggplot(aes(x = dbt, y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  geom_vline( aes(xintercept = 0))  +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (mg/L)') +
  ggtitle('Oxygen, 1 Day') +
  xlab('Days Before Turnover') +
  guides(linetype = "none") +
  theme_bw()
o1

t1 <- df %>% 
  filter(horizon==1,
         variable=="temperature") %>% 
  ggplot(aes(x = dbt, y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  geom_vline( aes(xintercept = 0))  +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  ylim(0, 2.2) +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (°C)') +
  ggtitle('Temperature, 1 Day') +
  xlab('Days Before Turnover') +
  guides(linetype = "none") +
  theme_bw()
t1

o7 <- df %>% 
  filter(horizon==7,
         variable=="oxygen") %>% 
  ggplot(aes(x = dbt, y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  geom_vline( aes(xintercept = 0))  +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  ylim(0, 2.3) +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (mg/L)') +
  ggtitle('Oxygen, 7 Days') +
  xlab('Days Before Turnover') +
  guides(linetype = "none") +
  theme_bw()
o7

t7 <- df %>% 
  filter(horizon==7,
         variable=="temperature") %>% 
  ggplot(aes(x = dbt, y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  geom_vline( aes(xintercept = 0))  +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (°C)') +
  ylim(0, 2.2) +
  ggtitle('Temperature, 7 Days') +
  xlab('Days Before Turnover') +
  guides(linetype = "none") +
  theme_bw()
t7

ggarrange(t1, t7, o1, o7, common.legend = TRUE, nrow = 1)

o21 <- df %>% 
  filter(horizon==21,
         variable=="oxygen") %>% 
  ggplot(aes(x = dbt, y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  geom_vline( aes(xintercept = 0))  +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  ylim(0, 2.3) +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (mg/L)') +
  ggtitle('Oxygen, 21 Days') +
  xlab('Days Before Turnover') +
  guides(linetype = "none") +
  theme_bw()
o21

t21 <- df %>% 
  filter(horizon==21,
         variable=="temperature") %>% 
  ggplot(aes(x = dbt, y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  geom_vline( aes(xintercept = 0))  +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1) +
  ylab('CRPS (°C)') +
  ylim(0, 2.2) +
  ggtitle('Temperature, 21 Days') +
  xlab('Days Before Turnover') +
  guides(linetype = "none") +
  theme_bw()
t21

ggarrange(t1, t7, t21, common.legend = TRUE, nrow = 1, align = "v")
ggarrange(o1, o7, o21, common.legend = TRUE, nrow = 1, align = "v")

