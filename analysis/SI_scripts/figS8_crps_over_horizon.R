library(lubridate)
library(tidyverse)
library(ggpubr)
library(arrow)
library(scales)
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
  filter(horizon > 0,
         as.Date(reference_datetime) %in% buoy_dates) %>% 
  select(model_id, reference_datetime, datetime, horizon, depth, variable, everything()) 

# convert oxy crps and obs to mg/L
sc <- sc %>% 
  mutate(crps = ifelse(variable=='temperature', crps, (crps*32/1000)),
         mean = ifelse(variable=='temperature', mean, (mean*32/1000)),
         observation = ifelse(variable=='temperature', observation, (observation*32/1000)),
         variable = factor(variable,
                           levels = c('temperature', 'oxygen'), 
                           ordered = TRUE,
                           labels = c('temperature (C)', 'oxygen (mg/L)')))

### calculate rmse
sc <- sc %>% 
  group_by(depth, variable, horizon, datetime) %>% 
  mutate(rmse = rmse(observation, mean)) 

#### aggregate across horizon
mean_crps_horizon_depth_year <- sc %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, horizon, depth, year) %>% 
  mutate(mean_crps = mean(crps, na.rm = TRUE),
         mean_log = mean(logs, na.rm = TRUE),
         mean_rmse = mean(rmse, na.rm = TRUE),
         sd_crps = sd(crps, na.rm = TRUE),
         sd_log = sd(logs, na.rm = TRUE),
         sd_rmse = sd(rmse, na.rm = TRUE)) %>% 
  distinct(variable, horizon, depth, .keep_all = TRUE) %>% 
  select(variable, horizon, depth, mean_crps:sd_rmse)


#################################################################################
# plot rmse over horizon
o_crps <- ggplot(mean_crps_horizon_depth_year[mean_crps_horizon_depth_year$variable=='oxygen (mg/L)',], aes(x = horizon, y = mean_crps, color = as.factor(year))) +
  geom_line() +
  geom_ribbon(aes(ymax = mean_crps + sd_crps, ymin = mean_crps - sd_crps, 
                  col = as.factor(year),
                  fill = as.factor(year)),
              alpha = 0.5) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~depth, ncol = 1) +
  xlab('horizon (days into future)') +
  ylab('CRPS (mg/L)') +
  ggtitle('Oxygen') +
  labs(color = 'Year',
       fill = 'Year') +
  theme_bw()


t_crps <- ggplot(mean_crps_horizon_depth_year[mean_crps_horizon_depth_year$variable=='temperature (C)',], aes(x = horizon, y = mean_crps, color = as.factor(year))) +
  geom_line() +
  geom_ribbon(aes(ymax = mean_crps + sd_crps, ymin = mean_crps - sd_crps, 
                  col = as.factor(year),
                  fill = as.factor(year)),
              alpha = 0.5) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~depth, ncol = 1) +
  xlab('horizon (days into future)') +
  ylab('CRPS (°C)') +
  ggtitle('Temperature') +
  labs(color = 'Year',
       fill = 'Year') +
  theme_bw()


crps_all <- ggarrange(t_crps, o_crps, common.legend = TRUE)

ggsave('./figures/figS8.png', crps_all, width = 300, height = 150, 
       units = "mm", dpi = 300, scale = 1)
