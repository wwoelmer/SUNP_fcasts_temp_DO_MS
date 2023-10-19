# script to produce figures of observations during focal forecast years

library(tidyverse)
library(lubridate)
library(ggpubr)

setwd(here::here())
sim_name <- 'SUNP_fcasts_temp_DO'

tgts <- read.csv(paste0('./targets/sunp/', sim_name, '/sunp-targets-insitu.csv'))
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

a <- ggplot(data = obs_mgL, aes(x = as.Date(mo_day, format = "%m-%d"), y = observed*32/1000, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~depth, ncol = 1) +
  scale_x_date(date_labels = "%b") +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  ylab('DO (mg/L)') +
  xlab('Date') +
  labs(color = 'Year') +
  theme_bw()
a

b <- ggplot(data = obs_mgL, aes(x = as.factor(year), y = observed*32/1000)) +
  facet_wrap(~depth, ncol = 1) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  geom_violin(aes(group = year, fill = as.factor(year))) +
  stat_summary(fun = "median",
               geom = "point",
               color = "black")  +
  ylab('DO (mg/L)') +
  xlab('Year') +
  labs(fill = 'Year') +
  theme_bw()
b

ggarrange(a, b, common.legend = TRUE)

# mean, min, max by year
summ_o <- obs_mgL %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  mutate(obs_mgL = observed*32/1000) %>% 
  group_by(year, depth) %>% 
  dplyr::summarise(mean = mean(obs_mgL, na.rm = TRUE), 
                   median = median(obs_mgL, na.rm = TRUE),
                   min = min(obs_mgL, na.rm = TRUE),
                   max = max(obs_mgL, na.rm = TRUE),
                   range = abs(min - max))

summ_o

summ_o_depth <- obs_mgL %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  mutate(obs_mgL = observed*32/1000) %>% 
  group_by(depth) %>% 
  dplyr::summarise(mean = mean(obs_mgL, na.rm = TRUE), 
                   median = median(obs_mgL, na.rm = TRUE),
                   min = min(obs_mgL, na.rm = TRUE),
                   max = max(obs_mgL, na.rm = TRUE),
                   range = abs(min - max))

summ_o_depth
#################################################################################################################
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
  
t_a <- ggplot(data = temp[temp$depth==1 | temp$depth==10,], aes(x = as.Date(mo_day, format = "%m-%d"), y = observed, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~depth, ncol = 1) +
  xlab('Date') +
  ylab('Temp (C)') +
  labs(color = 'Year') +
  theme_bw()
t_a
#ggsave('./figures/timeseries_obs_temp.tiff', t_a, scale = 0.5, dpi = 300, unit = "mm", width = 225, height = 220)


t_b <- ggplot(data = temp[temp$depth==1 | temp$depth==10,], aes(x = as.factor(year), y = observed)) +
  facet_wrap(~depth, ncol = 1) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  geom_violin(aes(group = year, fill = as.factor(year))) +
  stat_summary(fun = "median",
               geom = "point",
               color = "black", 
               width = 0.5)  +
  ylab('Temp (C)') +
  xlab('Year') +
  labs(fill = 'Year') +
  theme_bw()


fig3 <- ggarrange(t_a, a, t_b, b, 
                  nrow = 1, common.legend = TRUE)
fig3
ggsave('./figures/fig3_obs.tiff', fig3, scale = 0.5, dpi = 300, unit = "mm", width = 400, height = 150)

#############################################################################################
##############################################################################################################
## extras
ss <- ggarrange(t_a, t_b, 
          a, b,
          common.legend = TRUE)
ggsave('./figures/timeseries_violin_obs.tiff', ss, scale = 0.5, dpi = 300, unit = "mm", width = 225, height = 250)
  
ts <- ggarrange(t_a, a, common.legend = TRUE)
ggsave('./figures/timeseries_obs.tiff', ts, scale = 0.5, dpi = 300, unit = "mm", width = 225, height = 190)

ggarrange(t_b, b, common.legend = TRUE)

# mean, min, max by year
summ_t <- temp %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(year, depth) %>% 
  dplyr::summarise(mean = mean(observed, na.rm = TRUE), 
                   min = min(observed, na.rm = TRUE),
                   max = max(observed, na.rm = TRUE),
                   range = abs(min - max))

summ_t_depth <- temp %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(depth) %>% 
  dplyr::summarise(mean = mean(observed, na.rm = TRUE), 
                   min = min(observed, na.rm = TRUE),
                   max = max(observed, na.rm = TRUE),
                   range = abs(min - max))
summ_t_depth

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
  filter(mix > 0) 

ggplot(t_dates, aes(x = year, y = as.Date(mo_day, format = "%m-%d"))) +
  geom_line() +
  geom_point(size = 4) +
  ylab('Date of first mixing at buoy')

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

o_dates <- obs_mgL %>% 
  filter(as.Date(time) %in% t_dates$time) %>% 
  mutate(time = as.Date(time)) %>% 
  rename(mix_day = mo_day)

# make data frame of oxy and mixing dates
obs_mgL$time <- as.Date(obs_mgL$time)
mix_oxy <- full_join(obs_mgL, t_dates)

mix_dates <- full_join(o_dates, t_dates)
write.csv(mix_dates, './mixing_dates.csv', row.names = FALSE)

t_mix <- ggplot(data = temp[temp$depth==1 | temp$depth==10,], aes(x = as.Date(mo_day, format = "%m-%d"), y = observed, color = as.factor(year))) +
  geom_line() +
  geom_point(data = mix_dates[mix_dates$variable=='temperature',], size = 3, color = 'black', shape = 8, aes(x = as.Date(mix_day, format = "%m-%d"), y = observed))  +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~depth, scales = 'free', ncol = 1) +
  xlab('Date') +
  ylab('Temp (C)') +
  labs(color = 'Year') +
  theme_bw()

o_mix <- ggplot(data = obs_mgL, aes(x = as.Date(mo_day, format = "%m-%d"), y = observed*32/1000, color = as.factor(year))) +
  geom_line() +
  geom_point(data = mix_dates[mix_dates$variable=='oxygen',], size = 3, color = 'black', shape = 8, aes(x = as.Date(mix_day, format = "%m-%d"), y = observed*32/1000))  +
  facet_wrap(~depth, scales = 'free', ncol = 1) +
  scale_x_date(date_labels = "%b") +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  ylab('DO (mg/L)') +
  xlab('Date') +
  labs(color = 'Year') +
  theme_bw()

ggarrange(t_mix, o_mix, common.legend = TRUE)

