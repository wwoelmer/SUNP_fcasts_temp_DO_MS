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
         observation = ifelse(variable=='temperature', observation, (observation*32/1000)))




###############################################################################################
## read in RW and calculate mean scores
rw_scores <- read.csv('./scores/sunp/RW_scores.csv')
rw_scores <- rw_scores %>% 
  mutate(datetime = as.Date(datetime)) %>% 
  mutate(crps_rw = ifelse(variable=='temperature', crps, crps*32/1000)) %>% 
  select(-crps)

mean_rw_skill <- rw_scores %>% 
  group_by(variable, depth) %>% 
  mutate(mean_crps = mean(crps_rw)) %>% 
  distinct(variable, depth, .keep_all = TRUE)

sc <- sc %>% 
  select(datetime:variable, crps) %>% 
  mutate(datetime = as.Date(datetime))

sc_rw <- full_join(sc, rw_scores, by = c('datetime', 'depth', 'variable', 'horizon'))
sc_rw <- sc_rw %>% 
  mutate(nCRPS = 1 - (crps/crps_rw),
         year = year(datetime)) %>% 
  select(datetime, horizon, depth, variable, year, model_id,
         nCRPS)

##################################################################################################
### read in climatology 
clim <- read.csv(file.path(lake_directory, 'scores/sunp/climatology_scores.csv'))
clim <- clim %>% 
  select(time, depth, variable, crps) %>% 
  rename(crps_clim = crps,
         datetime = time) %>% 
  mutate(datetime = as.Date(datetime),
         depth = as.numeric(depth))

ggplot(clim, aes(x = datetime, y = crps_clim)) +
  geom_line() +
  facet_wrap(depth~variable, scales = 'free')

mean_clim_skill <- clim %>% 
  group_by(variable, depth) %>% 
  mutate(mean_crps = mean(crps_clim)) %>% 
  distinct(variable, depth, .keep_all = TRUE)

sc_clim <- full_join(sc, clim, by = c('datetime', 'depth', 'variable'))
sc_clim <- sc_clim %>% 
  mutate(nCRPS = 1 - (crps/crps_clim),
         model_id = 'clim',
         year = year(datetime)) %>% 
  select(-c(crps, crps_clim))


sc_all <- full_join(sc_rw, sc_clim, by = c('datetime', 'depth', 'variable', 'horizon', 
                                           'model_id', 'year', 'nCRPS'))

sc_all <- na.omit(sc_all)

mean_skill <- sc_all %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, horizon, depth, year, model_id) %>% 
  mutate(mean_crps = mean(nCRPS, na.rm = TRUE),
         sd_crps = sd(nCRPS, na.rm = TRUE)) %>% 
  distinct(variable, horizon, depth, year, model_id, .keep_all = TRUE) %>% 
  select(variable, horizon, depth, mean_crps:sd_crps)

mean_skill_no_horizon <- sc_all %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, depth, year, model_id) %>% 
  mutate(mean_crps = mean(nCRPS, na.rm = TRUE),
         sd_crps = sd(nCRPS, na.rm = TRUE)) %>% 
  distinct(variable, depth, year, model_id, .keep_all = TRUE) %>% 
  select(variable, depth, mean_crps:sd_crps)
##########################################################################################################
#### plot forecast skill

### persistence
rw_title <- expression(Skill[Persistence])
skill_fig_rw <- ggplot(mean_skill[mean_skill$model_id=='RW',], aes(x = horizon, y = mean_crps, color = as.factor(year))) +
  geom_line(linetype = 'dashed') +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  geom_ribbon(aes(ymax = mean_crps + sd_crps, ymin = mean_crps - sd_crps, fill = as.factor(year)), alpha = 0.5,
              linetype = 'dashed') +
  facet_grid(depth~fct_rev(variable)) +
  geom_hline(aes(yintercept = 0)) +
  ylab("Forecast Skill") +
  theme_bw() +
  theme(panel.spacing = unit(0.5, "cm")) +
  labs(fill = 'Year', linetype = 'Variable') +
  guides(color = FALSE) +
  ggtitle(rw_title)
skill_fig_rw

#### climatology
clim_title <- expression(Skill[Climatology])
skill_fig_clim <- ggplot(mean_skill[mean_skill$model_id=='clim',], aes(x = horizon, y = mean_crps, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  geom_ribbon(aes(ymax = mean_crps + sd_crps, ymin = mean_crps - sd_crps, fill = as.factor(year)), alpha = 0.5) +
  facet_grid(depth~fct_rev(variable)) +
  geom_hline(aes(yintercept = 0)) +
  ylab("Forecast Skill") +
  theme_bw() +
  theme(panel.spacing = unit(0.5, "cm")) +
  labs(fill = 'Year', linetype = 'Variable') +
  guides(color = FALSE) +
  ggtitle(clim_title)
skill_fig_clim

fig5 <- ggarrange(skill_fig_clim, skill_fig_rw, common.legend = TRUE)
fig5
ggsave('./figures/fig5_year_depth_variable_skill.tiff', fig5, scale = 0.5, dpi = 300, unit = "mm", width = 335, height = 200)


#######################################################################################################################
#######################################################################################################################
# extra fig
# all together
skill_fig <- ggplot(mean_skill, aes(x = horizon, y = mean_crps, linetype = model_id, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  #facet_wrap(~depth, ncol = 1) +
  #geom_ribbon(aes(ymax = mean_crps + sd_crps, ymin = mean_crps - sd_crps, fill = as.factor(year)), alpha = 0.5) +
  facet_grid(depth~fct_rev(variable)) +
  geom_hline(aes(yintercept = 0)) +
  ylab("Forecast Skill") +
  theme_bw() +
  theme(panel.spacing = unit(0.5, "cm")) +
  labs(color = 'Year', linetype = 'Variable') +
  guides(fill = FALSE)
skill_fig
