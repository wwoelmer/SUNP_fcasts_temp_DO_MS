
library(lubridate)
library(tidyverse)
library(ggpubr)
library(arrow)
library(scales)
library(rMR)
library(scoringRules)
library(Metrics)

lake_directory <- here::here()

# some subsetting variables
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)
horizons <- c(1:35)
sim_name <- 'SUNP_fcasts_temp_DO' 

########################################################################
# read in the scores and calculate variance
score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp", sim_name))

sc <- arrow::open_dataset(score_dir) |> 
  filter(variable %in% vars,
         depth %in% depths) %>% 
  collect() 


# vector of dates when obs are available (before buoy is taken into harbor)
##### make these the same dates for each year for equal comparison
buoy_dates <- c(seq.Date(as.Date('2021-08-04'), as.Date('2021-10-17'), by = 'day'),
                seq.Date(as.Date('2022-08-04'), as.Date('2022-10-17'), by = 'day'))

sc <- sc %>% 
  mutate(doy = yday(datetime),
         year = year(datetime)) %>% 
  select(-c(family, site_id)) %>% 
  filter(horizon > 0,#) %>% 
         as.Date(reference_datetime) %in% buoy_dates) %>% 
  select(model_id, reference_datetime, datetime, horizon, depth, variable, everything()) 

# convert oxy crps and obs to mg/L
sc <- sc %>% 
  mutate(crps = ifelse(variable=='temperature', crps, (crps*32/1000)),
         observation = ifelse(variable=='temperature', observation, (observation*32/1000)),
         mean = ifelse(variable=='temperature', mean, (mean*32/1000)),
         sd = ifelse(variable=='temperature', sd, (sd*32/1000)))

sc$variable <- factor(sc$variable, levels = c('temperature', 'oxygen'), 
                      ordered = TRUE, labels = c('temperature (C)', 'oxygen (mg/L)'))

#######################################################################
## calculate % saturation instead of mg/L
ox <- sc %>% 
  filter(variable=='oxygen (mg/L)') %>% 
  select(model_id:observation, mean, sd) %>% 
  mutate(row = row_number())%>%   
  pivot_wider(names_from = variable, values_from = c(observation, mean, sd))

temp <- sc %>% 
  filter(variable=='temperature (C)') %>% 
  select(model_id:observation) %>% 
  mutate(row = row_number())%>%   
  pivot_wider(names_from = variable, values_from = c(observation))

sat <- left_join(ox, temp) 
sat <- sat %>% 
  dplyr::rename('obs_mgL' = "observation_oxygen (mg/L)",
                'mean_mgL' = "mean_oxygen (mg/L)",
                'sd_mgL' = "sd_oxygen (mg/L)",
                'temp_C' = "temperature (C)") %>% 
  mutate(label = ifelse(depth==1, "1.0 m", "10.0 m"))

sunp_elev <- 333 #m (1093 feet)
sat <- sat %>% 
  mutate(obs_sat = DO.saturation(obs_mgL, temp_C, elevation.m = sunp_elev)*100,
         mean_sat = DO.saturation(mean_mgL, temp_C, elevation.m = sunp_elev)*100,
         sd_sat = DO.saturation(sd_mgL, temp_C, elevation.m = sunp_elev)*100,
         year = year(datetime),
         doy = yday(datetime),
         mo_day = format(as.Date(datetime), "%m-%d"))

a <- ggplot(data = sat, aes(x = as.Date(mo_day, format = "%m-%d"), y = obs_sat, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~label, scales = 'free') +
  scale_x_date(date_labels = "%b") +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  ylab('Oxygen (% Saturation)') +
  xlab('Date') +
  labs(color = 'Year')

b <- ggplot(data = sat, aes(x = as.factor(year), y = obs_sat)) +
  facet_wrap(~label) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  geom_boxplot(aes(group = year, fill = as.factor(year))) +
  ylab('Oxygen (% Saturation)') +
  xlab('Year') +
  labs(fill = 'Year')

c <- ggplot(data = sat, aes(x = as.Date(mo_day, format = "%m-%d"), y = obs_mgL, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~label, scales = 'free') +
  scale_x_date(date_labels = "%b") +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  ylab('Oxygen (mg/L)') +
  xlab('Date') +
  labs(color = 'Year')

d <- ggplot(data = sat, aes(x = as.factor(year), y = obs_mgL)) +
  facet_wrap(~label) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  geom_boxplot(aes(group = year, fill = as.factor(year))) +
  ylab('Oxygen (mg/L)') +
  xlab('Year') +
  labs(fill = 'Year')


dofig <- ggarrange(a, b, c, d, common.legend = TRUE)
dofig
ggsave('./figures/fig_S5.png', dofig, scale = 0.7, dpi = 300, unit = "mm", width = 325, height = 220)
