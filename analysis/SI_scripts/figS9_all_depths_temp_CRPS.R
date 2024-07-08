# plot scores across all depths
library(lubridate)
library(tidyverse)
library(ggpubr)
library(arrow)
library(scales)
library(Metrics)

lake_directory <- here::here()

# some subsetting variables
vars <- c('temperature')
depths <- c(0.1, seq(1.0, 10.0))
horizons <- c(1:35)
sim_name <- 'SUNP_fcasts_temp_DO' 
#folders <- c('all_UC_fsed_deep_DA')

########################################################################
# read in the scores and calculate variance
score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp", sim_name))

sc <- arrow::open_dataset(score_dir) |> 
  filter(variable %in% vars,
         depth %in% depths) %>% 
  collect() 


# vector of dates when obs are available (before buoy is taken into harbor)
##### subset to the same time frame for each year for equal comparison
buoy_dates <- c(seq.Date(as.Date('2021-08-04'), as.Date('2021-10-17'), by = 'day'),
                seq.Date(as.Date('2022-08-04'), as.Date('2022-10-17'), by = 'day'))

sc <- sc %>% 
  mutate(doy = yday(datetime),
         year = year(datetime)) %>% 
  select(-c(family, site_id)) %>% 
  filter(horizon > 0,
         as.Date(reference_datetime) %in% buoy_dates) %>% 
  select(model_id, reference_datetime, datetime, horizon, depth, variable, everything()) 


# mean CRPS across depth by year and horizon
mean_crps <- sc %>% 
  group_by(variable, horizon, depth, year) %>% 
  mutate(mean_crps = mean(crps, na.rm = TRUE),
         sd_crps = sd(crps, na.rm = TRUE)) %>% 
  distinct(variable, horizon, depth, .keep_all = TRUE) %>% 
  select(variable, horizon, depth, mean_crps:sd_crps)

dpts <- ggplot(mean_crps, aes(x = horizon, y = mean_crps, color = as.factor(depth))) +
  geom_point() +
  geom_line() +
  facet_wrap(~year) +
  theme_bw() +
  ylab(expression(paste("Mean CRPS (", degree, "C)"))) + # Add degree symbol to y-axis label
  labs(color = 'Depth (m)')
dpts

ggsave('./figures/figS9_all_depths_temp.tiff', dpts, scale = 0.6, dpi = 300, 
       unit = "mm", width = 400, height = 250)

