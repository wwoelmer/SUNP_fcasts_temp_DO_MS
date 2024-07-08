# plot all historical buoy data for temperature and oxygen for SI fig

library(lubridate)
library(tidyverse)
library(ggpubr)
library(scales)

setwd(here::here())
sim_name <- 'SUNP_fcasts_temp_DO'

tgts <- read.csv(paste0('./targets/sunp/', sim_name, '/sunp-targets-insitu.csv'))
depths <- c(1.0, 10.0)

oxy <- tgts %>% 
  select(time, depth, observed, variable) %>% 
  filter(variable=='oxygen',
         observed > 0)

# round 1.5 data to 1.0m
oxy <- oxy %>% 
  mutate(depth_cor = ifelse(depth==1.5, 1.0, depth)) %>% 
  mutate(depth = depth_cor) %>% 
  select(-depth_cor) %>% 
  filter(depth %in% depths) 

oxy <- oxy %>% 
  mutate(year = year(time),
         mo_day = format(as.Date(time), "%m-%d")) %>% 
  filter(mo_day > "06-29" & mo_day < '10-18',
         year > 2006)

# remove years with buoy data issues
weird_years <- c('2007', '2008', '2013', '2014', '2017')
oxy <- oxy %>% 
  filter(!year %in% weird_years)

# create color scale
grad <- seq_gradient_pal('lightgray', 'darkgreen', 'Lab')(seq(0, 1, length.out = 13))
colsq <- c(grad,  c('#17BEBB', '#9E2B25'))
years <- seq(2008, 2022)

col_years <- data.frame(colsq, years)
cols_oxy <- col_years[which(col_years$years %in% oxy$year),]

a <- ggplot(data = oxy, aes(x = as.Date(mo_day, format = "%m-%d"), y = observed*32/1000, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~depth, scales = 'free') +
  scale_x_date(date_labels = "%b") +
  scale_color_manual(values = cols_oxy[,1]) +
  ylab('Oxygen (mg/L)') +
  xlab('Date') +
  labs(color = 'Year') +
  theme_bw()

b <- ggplot(data = oxy, aes(x = as.factor(year), y = observed*32/1000)) +
  facet_wrap(~depth) +
  geom_boxplot(aes(group = year, fill = as.factor(year))) +
  scale_fill_manual(values = cols_oxy[,1]) +
  ylab('Oxygen (mg/L)') +
  xlab('Year') +
  labs(fill = 'Year') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme_bw()


oxy_hist_fig <- ggarrange(a, b, common.legend = TRUE)
oxy_hist_fig
ggsave('./figures/fig_S11.png', oxy_hist_fig, width = 500, height = 250, 
       units = "mm", dpi = 300, scale = 0.5)

## calculate summary statistics
hist_o <- oxy %>% 
  filter(depth %in% c(1.0, 10.0),
         year < 2021) %>% 
  mutate(obs_mgL = observed*32/1000) %>% 
  group_by(depth) %>% 
  dplyr::summarise(mean = mean(obs_mgL, na.rm = TRUE), 
                   median = median(obs_mgL, na.rm = TRUE),
                   min = min(obs_mgL, na.rm = TRUE),
                   max = max(obs_mgL, na.rm = TRUE),
                   range = abs(min - max))
hist_o

hist_o_yrly <- oxy %>% 
  filter(depth %in% c(1.0, 10.0),
         year < 2021) %>% 
  mutate(obs_mgL = observed*32/1000) %>% 
  group_by(depth, year) %>% 
  dplyr::summarise(mean = mean(obs_mgL, na.rm = TRUE), 
                   median = median(obs_mgL, na.rm = TRUE),
                   min = min(obs_mgL, na.rm = TRUE),
                   max = max(obs_mgL, na.rm = TRUE),
                   range = abs(min - max))
hist_o_yrly
#################################################################################################################
# historical temperature
temp <- tgts %>% 
  select(time, depth, observed, variable) %>% 
  filter(variable=='temperature') %>% 
  #  filter(as.Date(time) %in% buoy_dates) %>% 
  mutate(year = year(time),
         mo_day = format(as.Date(time), "%m-%d"))

# round 1.5 data to 1.0m, and select only buoy data (after 2007)
temp <- temp %>% 
  mutate(depth_cor = ifelse(depth==1.5, 1.0, depth)) %>% 
  mutate(depth = depth_cor) %>% 
  select(-depth_cor) %>% 
  filter(depth %in% depths,
         time > '2008-01-01' & time < '2023-01-01')


## limit to the same time duration between years
temp <- temp %>% 
  filter(mo_day > "06-08" & mo_day < '10-18')

# remove years with funky stuff going on
weird_years <- c('2007', '2009', '2013', '2017')
temp <- temp %>% 
  filter(!year %in% weird_years)

cols_temp <- col_years[which(col_years$years %in% temp$year),]

t_a <- ggplot(data = temp[temp$depth==1 | temp$depth==10,], aes(x = as.Date(mo_day, format = "%m-%d"), y = observed, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = cols_temp[,1]) +
  facet_wrap(~depth, scales = 'free') +
  xlab('Date') +
  ylab('Temperature (°C)') +
  labs(color = 'Year') +
  theme_bw()


t_b <- ggplot(data = temp[temp$depth==1 | temp$depth==10,], aes(x = as.factor(year), y = observed)) +
  facet_wrap(~depth) +
  scale_fill_manual(values = cols_temp[,1]) +
  geom_boxplot(aes(group = year, fill = as.factor(year))) +
  ylab('Temperature (°C)') +
  xlab('Year') +
  labs(fill = 'Year') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) 
  

temp_hist_fig <- ggarrange(t_a, t_b, common.legend = TRUE)
temp_hist_fig
ggsave('./figures/fig_s15.png', temp_hist_fig, width = 500, height = 250, 
       units = "mm", dpi = 300, scale = 0.5)

#################################################################################################
## summary statistics
hist_t <- temp %>% 
  filter(depth %in% c(1.0, 10.0),
         year < 2021) %>% 
  group_by(depth) %>% 
  dplyr::summarise(mean = mean(observed, na.rm = TRUE), 
                   median = median(observed, na.rm = TRUE),
                   min = min(observed, na.rm = TRUE),
                   max = max(observed, na.rm = TRUE),
                   range = abs(min - max))
hist_t
