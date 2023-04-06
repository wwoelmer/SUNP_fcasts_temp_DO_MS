# plot all historical buoy data for temperature and oxygen

library(lubridate)
library(tidyverse)
library(ggpubr)
#install.packages('plotly')
library(plotly)


setwd(here::here())

tgts <- read.csv('./targets/sunp/UC_analysis_2022/sunp-targets-insitu.csv')

oxy <- tgts %>% 
  select(time, depth, observed, variable) %>% 
  filter(variable=='oxygen',
         observed > 0) 

oxy <- oxy %>% 
  mutate(year = year(time),
         mo_day = format(as.Date(time), "%m-%d")) %>% 
  filter(mo_day > "06-29" & mo_day < '10-18')

# round 1.5 data to 1.0m
oxy <- oxy %>% 
  mutate(depth_cor = ifelse(depth==1.5, 1.0, depth)) %>% 
  mutate(depth = depth_cor) %>% 
  select(-depth_cor)

# remove years with funky stuff going on
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
  ylab('DO (mg/L)') +
  xlab('Date') +
  labs(color = 'Year')
a
ggplotly(a)

b <- ggplot(data = oxy, aes(x = as.factor(year), y = observed*32/1000)) +
  facet_wrap(~depth) +
  geom_boxplot(aes(group = year, fill = as.factor(year))) +
  scale_fill_manual(values = cols_oxy[,1]) +
  ylab('DO (mg/L)') +
  xlab('Year') +
  labs(fill = 'Year') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
b

ggarrange(a, b, common.legend = TRUE)

#################################################################################################################
temp <- tgts %>% 
  select(time, depth, observed, variable) %>% 
  filter(variable=='temperature') %>% 
  #  filter(as.Date(time) %in% buoy_dates) %>% 
  mutate(year = year(time),
         mo_day = format(as.Date(time), "%m-%d"))

# round 1.5 data to 1.0m
temp <- temp %>% 
  mutate(depth_cor = ifelse(depth==1.5, 1.0, depth)) %>% 
  mutate(depth = depth_cor) %>% 
  select(-depth_cor)


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
  ylab('Temp (C)') +
  labs(color = 'Year')
t_a
#ggplotly(t_a)

t_b <- ggplot(data = temp[temp$depth==1 | temp$depth==10,], aes(x = as.factor(year), y = observed)) +
  facet_wrap(~depth) +
  scale_fill_manual(values = cols_temp[,1]) +
  geom_boxplot(aes(group = year, fill = as.factor(year))) +
  ylab('Temp (C)') +
  xlab('Year') +
  labs(fill = 'Year') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
t_b

ggarrange(t_a, t_b, common.legend = TRUE)

