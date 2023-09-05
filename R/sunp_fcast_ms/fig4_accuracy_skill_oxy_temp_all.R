#install.packages('Metrics')

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
sim_name <- 'SUNP_fsed_deep_DA' # UC_analysis_2021/start_06_30
folders <- c('all_UC_fsed_deep_DA')

########################################################################
# read in the scores and calculate variance
score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp", sim_name, folders[1]))

sc <- arrow::open_dataset(score_dir) |> 
  filter(variable %in% vars,
         depth %in% depths) %>% 
  collect() 

for(i in 1:length(folders)){
  score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp", sim_name, folders[1]))
  
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
  filter(horizon > 0,
         as.Date(reference_datetime) %in% buoy_dates) %>% 
  select(model_id, reference_datetime, datetime, horizon, depth, variable, everything()) 

# convert oxy crps and obs to mg/L
sc <- sc %>% 
  mutate(crps = ifelse(variable=='temperature', crps, (crps*32/1000)),
         mean = ifelse(variable=='temperature', mean, (mean*32/1000)),
         observation = ifelse(variable=='temperature', observation, (observation*32/1000)))

#### aggregate across horizon
mean_skill_horizon_depth_year <- sc %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, horizon, depth, year) %>% 
  mutate(mean_crps = mean(crps, na.rm = TRUE),
         sd_crps = sd(crps, na.rm = TRUE)) %>% 
  distinct(variable, horizon, depth, .keep_all = TRUE) %>% 
  select(variable, horizon, depth, mean_crps:sd_crps)

mean_skill_horizon <- sc %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, horizon) %>% 
  mutate(mean_crps = mean(crps, na.rm = TRUE),
         sd_crps = sd(crps, na.rm = TRUE)) %>% 
  distinct(variable, horizon, .keep_all = TRUE) %>% 
  select(variable, horizon, mean_crps:sd_crps)

temp_fig <- ggplot(mean_skill_horizon[mean_skill_horizon$variable=='temperature',], 
                   aes(x = horizon, y = mean_crps, linetype = as.factor(variable))) +
  geom_line() +
  facet_wrap(~variable, scales = 'free') +
  theme_bw() +
  xlab('Forecast Horizon (days)') +
  ylab('CRPS (°C)') +
  guides(linetype = "none")
temp_fig

oxy_fig <- ggplot(mean_skill_horizon[mean_skill_horizon$variable=='oxygen',], 
                   aes(x = horizon, y = mean_crps, linetype = as.factor(variable))) +
  geom_line() +
  facet_wrap(~variable, scales = 'free') +
  theme_bw() +
  xlab('Forecast Horizon (days)') +
  ylab('CRPS (mg/L)') +
  guides(linetype = "none")
oxy_fig

ggarrange(temp_fig, oxy_fig, labels = 'auto')

###############################################################################################
# calculate climatology
########################################################################
# read in climatology and calculate mean scores
clim <- read.csv(file.path(lake_directory, 'scores/sunp/climatology_scores.csv'))
clim <- clim %>% 
  select(time, depth, variable, crps) %>% 
  rename(crps_clim = crps,
         datetime = time) %>% 
  mutate(datetime = as.Date(datetime),
         depth = as.numeric(depth))

sc <- sc %>% 
  select(datetime:variable, crps) %>% 
  mutate(datetime = as.Date(datetime))

sc_clim <- full_join(sc, clim, by = c('datetime', 'depth', 'variable'))
sc_clim <- sc_clim %>% 
  mutate(nCRPS = 1 - (crps/crps_clim))


###########################################################################################
## read in persistence
rw_scores <- read.csv('./scores/sunp/RW_scored.csv')
rw_scores <- rw_scores %>% 
  mutate(datetime = as.Date(datetime)) %>% 
  mutate(crps_rw = ifelse(variable=='temperature', crps, crps*32/1000)) %>% 
  select(-crps)
  

sc_rw <- full_join(sc, rw_scores, by = c('datetime', 'depth', 'variable', 'horizon'))
sc_rw <- sc_rw %>% 
  mutate(nCRPS = 1 - (crps/crps_rw))

mean_hzon_var_rw <- sc_rw %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, horizon) %>% 
  mutate(mean_crps = mean(nCRPS, na.rm = TRUE),
         sd_crps = sd(nCRPS, na.rm = TRUE)) %>% 
  distinct(variable, horizon, depth, .keep_all = TRUE) %>% 
  select(variable, horizon, depth, mean_crps:sd_crps)

skill_fig <- ggplot(mean_hzon_var_rw, aes(x = horizon, y = mean_crps, linetype = variable)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 0)) +
  #geom_ribbon(aes(ymax = mean_crps + sd_crps, ymin = mean_crps - sd_crps, fill = variable), alpha = 0.2) +
  ylab("Forecast Skill") +
  xlab('Forecast Horizon (days)') +
  theme_bw() 
skill_fig


#### aggregate across horizon
mean_hzon_var <- sc_clim %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, horizon) %>% 
  mutate(mean_crps = mean(nCRPS, na.rm = TRUE),
         sd_crps = sd(nCRPS, na.rm = TRUE)) %>% 
  distinct(variable, horizon, depth, .keep_all = TRUE) %>% 
  select(variable, horizon, depth, mean_crps:sd_crps)

skill_fig <- ggplot(mean_hzon_var, aes(x = horizon, y = mean_crps, linetype = variable, color = 'climatology')) +
  geom_line(size = 1) +
  geom_line(data = mean_hzon_var_rw, aes(x = horizon, y = mean_crps, linetype = variable, color = 'RW'), size = 1) +
  geom_hline(aes(yintercept = 0)) +
  ylab("Forecast Skill") +
  xlab('Forecast Horizon (days)') +
  theme_bw() 
skill_fig

all_fig <- ggarrange(temp_fig, oxy_fig, skill_fig, labels = 'auto', nrow = 1, widths = c(0.6, 0.6, 1))
all_fig
ggsave('./figures/accuracy_skill_temp_oxy.tiff', all_fig, scale = 0.4, dpi = 300, unit = "mm", width = 525, height = 150)
