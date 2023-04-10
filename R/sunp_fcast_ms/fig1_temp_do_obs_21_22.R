library(tidyverse)
library(lubridate)
library(ggpubr)

setwd(here::here())

tgts <- read.csv('./targets/sunp/UC_analysis_2022/sunp-targets-insitu.csv')
years <- c('2021', '2022')
obs <- tgts %>% 
  mutate(year = year(time)) %>% 
  filter(year %in% years)

obs <- obs %>% 
  select(time, depth, observed, variable) %>% 
  filter(variable=='oxygen',
         observed > 0) 

# limit to the days that we will analyze for forecasts (i.e., when we have NOAA forecasts for)
buoy_dates <- c(seq.Date(as.Date('2021-06-29'), as.Date('2021-10-19'), by = 'day'),
                seq.Date(as.Date('2022-04-26'), as.Date('2022-10-17'), by = 'day'))

obs <- obs %>% 
  filter(as.Date(time) %in% buoy_dates) %>% 
  mutate(year = year(time),
         mo_day = format(as.Date(time), "%m-%d")) %>% 
  filter(mo_day > "06-29" & mo_day < '10-18')

obs_mgL <- obs[obs$variable=='oxygen',]

a <- ggplot(data = obs_mgL, aes(x = as.Date(mo_day, format = "%m-%d"), y = observed*32/1000, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~depth, scales = 'free') +
  scale_x_date(date_labels = "%b") +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  ylab('DO (mg/L)') +
  xlab('Date') +
  labs(color = 'Year')
a
b <- ggplot(data = obs_mgL, aes(x = as.factor(year), y = observed*32/1000)) +
  facet_wrap(~depth) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  geom_boxplot(aes(group = year, fill = as.factor(year))) +
  ylab('DO (mg/L)') +
  xlab('Year') +
  labs(fill = 'Year')

ggarrange(a, b, common.legend = TRUE)

#################################################################################################################
temp <- read.csv('./targets/sunp/UC_analysis_2022/sunp-targets-insitu.csv')
temp <- temp %>% 
  select(time, depth, observed, variable) %>% 
  filter(variable=='temperature') %>% 
#  filter(as.Date(time) %in% buoy_dates) %>% 
  mutate(year = year(time),
         mo_day = format(as.Date(time), "%m-%d")) %>% 
  filter(year %in% years,
         as.Date(time) > as.Date('2021-06-08'))

# round 1.5 data to 1.0m
temp <- temp %>% 
  mutate(depth_cor = ifelse(depth==1.5, 1.0, depth)) %>% 
  mutate(depth = depth_cor) %>% 
  select(-depth_cor)

ggplot(data = temp[temp$year==2021,], aes(x = as.Date(mo_day, format = "%m-%d"), y = observed, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~depth)

ggplot(data = temp, aes(x = as.Date(mo_day, format = "%m-%d"), y = observed, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~depth)

## limit to the same time duration between years
temp <- temp %>% 
  filter(mo_day > "06-08" & mo_day < '10-18')

t_a <- ggplot(data = temp[temp$depth==1 | temp$depth==10,], aes(x = as.Date(mo_day, format = "%m-%d"), y = observed, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~depth, scales = 'free') +
  xlab('Date') +
  ylab('Temp (C)') +
  labs(color = 'Year')

t_b <- ggplot(data = temp[temp$depth==1 | temp$depth==10,], aes(x = as.factor(year), y = observed)) +
  facet_wrap(~depth) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  geom_boxplot(aes(group = year, fill = as.factor(year))) +
  ylab('Temp (C)') +
  xlab('Year')
ggarrange(t_a, t_b, common.legend = TRUE)

ggarrange(t_a, t_b, 
          a, b,
          common.legend = TRUE)


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
#filter(year==2022,
#       mo_day > '09-20') 

t_dates <- to %>% 
  filter(mo_day > '08-10') %>% 
  select(year:depth10) %>% 
  group_by(year, mix) %>% 
  slice(1) %>% 
  filter(mix > 0) %>% 
  select(year, mo_day, depth_surf, depth10, t_diff, mix) 

ggplot(t_dates, aes(x = year, y = as.Date(mo_day, format = "%m-%d"))) +
  geom_line() +
  geom_point(size = 4) +
  ylab('Date of first mixing at buoy')

# 2019-09-14
# 2020-09-07
# 2021-10-04
# 2022-09-23


t_dates <- t_dates %>% 
  rename(mix_day = mo_day) %>% 
  select(year, mix_day, t_diff, mix)

# make data frame of oxy and mixing dates
mix_oxy <- left_join(obs_mgL, t_dates)

c <- ggplot(data = mix_oxy, aes(x = as.Date(mo_day, format = "%m-%d"), y = observed, color = as.factor(year))) +
  geom_line() +
  geom_vline(aes(xintercept = as.Date(mix_day, format = "%m-%d"), color = as.factor(year))) +
  facet_wrap(~depth)
c

