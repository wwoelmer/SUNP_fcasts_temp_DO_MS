# script to produce figures of observations during focal forecast years

library(lubridate)
library(tidyverse)
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
  filter(#variable=='oxygen',
    observed > 0) 

# limit to the days that we will analyze for forecasts (i.e., when we have NOAA forecasts for)
buoy_dates <- c(seq.Date(as.Date('2021-08-04'), as.Date('2021-10-17'), by = 'day'),
                seq.Date(as.Date('2022-08-04'), as.Date('2022-10-17'), by = 'day'))

mix_21 <- "10-04"
mix_22 <- "09-23"


obs <- obs %>% 
  filter(as.Date(time) %in% buoy_dates) %>% 
  mutate(year = year(time),
         mo_day = format(as.Date(time), "%m-%d")) %>% 
  filter(mo_day > "06-29" & mo_day < '10-18') %>% 
  mutate(label = ifelse(depth==1, "1.0 m", "10.0 m")) %>% 
  mutate(mix_date = ifelse(year=='2021', '10-04', '09-23')) %>% 
  group_by(depth, variable, year) %>% 
  mutate(mix_val = observed[mo_day==mix_date])


obs_mgL <- obs[obs$variable=='oxygen',]
temp_C <- obs[obs$variable=='temperature',]

##################################################################################################
# oxygen plots

a <- ggplot(data = obs_mgL, aes(x = as.Date(mo_day, format = "%m-%d"), y = observed*32/1000, 
                                color = as.factor(year))) +
  geom_line() +
  facet_wrap(~label, ncol = 1) +
  scale_x_date(date_labels = "%b") +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  ylab('Dissolved oxygen (mg/L)') +
  xlab('Date') +
  geom_point(aes(y = mix_val*32/1000, x = as.Date(mix_date, format = "%m-%d")), shape = 15, size = 2) +
  labs(color = 'Year',
       shape = 'Turnover') +
  theme_bw() +
  guides(size = 'none')
a

b <- ggplot(data = obs_mgL, aes(x = as.factor(year), y = observed*32/1000)) +
  facet_wrap(~label, ncol = 1) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  geom_violin(aes(group = year, fill = as.factor(year))) +
  stat_summary(fun = "median",
               geom = "point",
               color = "black")  +
  ylab('Dissolved oxygen (mg/L)') +
  xlab('Year') +
  labs(fill = 'Year') +
  theme_bw()
b

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
# temperature plots

t_a <- ggplot(data = temp_C, aes(x = as.Date(mo_day, format = "%m-%d"), y = observed, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~label, ncol = 1) +
  geom_point(aes(y = mix_val, x = as.Date(mix_date, format = "%m-%d"), 
  ), shape = 15, size = 2) +
  xlab('Date') +
  ylab('Temperature (°C)') +
  labs(color = 'Year',
       shape = 'Turnover') +
  theme_bw() +
  guides(size = 'none')
t_a

t_b <- ggplot(data = temp_C, aes(x = as.factor(year), y = observed)) +
  facet_wrap(~label, ncol = 1) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  geom_violin(aes(group = year, fill = as.factor(year))) +
  stat_summary(fun = "median",
               geom = "point",
               color = "black")  +
  ylab('Temperature (°C)') +
  xlab('Year') +
  labs(fill = 'Year') +
  theme_bw()
t_b

fig3 <- ggarrange(t_a, a, t_b, b, 
                  nrow = 1, common.legend = TRUE)
fig3
ggsave('./figures/fig3.tiff', fig3, scale = 0.5, dpi = 300, unit = "mm", width = 400, height = 150)

# mean, min, max by year
summ_t <- temp_C %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(year, depth) %>% 
  dplyr::summarise(mean = mean(observed, na.rm = TRUE), 
                   min = min(observed, na.rm = TRUE),
                   max = max(observed, na.rm = TRUE),
                   range = abs(min - max))

summ_t_depth <- temp_C %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(depth) %>% 
  dplyr::summarise(mean = mean(observed, na.rm = TRUE), 
                   min = min(observed, na.rm = TRUE),
                   max = max(observed, na.rm = TRUE),
                   range = abs(min - max))
summ_t_depth

