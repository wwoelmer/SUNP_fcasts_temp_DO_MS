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

sc$variable <- factor(sc$variable, levels = c('temperature', 'oxygen'), 
                      ordered = TRUE, labels = c('temperature (C)', 'oxygen (mg/L)'))


#### aggregate across horizon
mean_skill_horizon_depth_year <- sc %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, horizon, depth, year) %>% 
  mutate(mean_crps = mean(crps, na.rm = TRUE),
         mean_log = mean(logs, na.rm = TRUE),
         sd_crps = sd(crps, na.rm = TRUE),
         sd_log = sd(logs, na.rm = TRUE)) %>% 
  distinct(variable, horizon, depth, .keep_all = TRUE) %>% 
  select(variable, horizon, depth, mean_crps:sd_log)

mean_skill_horizon <- sc %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, horizon) %>% 
  mutate(mean_crps = mean(crps, na.rm = TRUE),
         mean_log = mean(logs, na.rm = TRUE),
         sd_crps = sd(crps, na.rm = TRUE),
         sd_log = sd(logs, na.rm = TRUE)) %>% 
  distinct(variable, horizon, .keep_all = TRUE) %>% 
  select(variable, horizon, mean_crps:sd_log)

mean_skill_year <- sc %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, depth, year) %>% 
  mutate(mean_crps = mean(crps, na.rm = TRUE),
         mean_log = mean(logs, na.rm = TRUE),
         sd_crps = sd(crps, na.rm = TRUE),
         sd_log = sd(logs, na.rm = TRUE)) %>% 
  distinct(variable, depth, .keep_all = TRUE) %>% 
  select(variable, depth, mean_crps)

perf_wide <- mean_skill_year %>% 
  pivot_wider(names_from = year, values_from = mean_crps)

ggplot(perf_wide) +
  geom_segment(aes(x=fct_rev(as.factor(depth)), xend=fct_rev(as.factor(depth)), y=`2021`, yend=`2022`), color="grey") +
  geom_point(aes(y=`2021`, x=fct_rev(as.factor(depth)), color = '2021'), size=3 ) +
  geom_point(aes(y=`2022`, x=fct_rev(as.factor(depth)), color = '2022'), size=3 ) +
  scale_y_reverse() +
  coord_flip()+
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~fct_rev(variable), ncol = 1, scales = 'free_x') +
  theme_bw() +
  xlab('Depth') +
  ylab('Performance') +
  labs(color = 'Year')

perf_wide$metric <- "Performance"

mean_skill_depth <- sc %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, year) %>% 
  mutate(mean_crps = mean(crps, na.rm = TRUE),
         mean_log = mean(logs, na.rm = TRUE),
         sd_crps = sd(crps, na.rm = TRUE),
         sd_log = sd(logs, na.rm = TRUE)) %>% 
  distinct(variable, depth, .keep_all = TRUE) %>% 
  select(variable, depth, mean_crps)

ggplot(mean_skill_depth, aes(x = as.factor(year), y = mean_crps, color = as.factor(year))) +
  geom_point(size = 3) +
  #geom_jitter(size = 3) +
  #ylim(0.5, 0.9) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~fct_rev(variable)) +
  theme_bw() +
  xlab('Year') +
  ylab('Mean CRPS') +
  labs(color = 'Variable',
       shape = 'Depth')

mean_skill <- sc %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, depth) %>% 
  mutate(mean_crps = mean(crps, na.rm = TRUE),
         mean_log = mean(logs, na.rm = TRUE),
         sd_crps = sd(crps, na.rm = TRUE),
         sd_log = sd(logs, na.rm = TRUE)) %>% 
  distinct(variable, depth, .keep_all = TRUE) %>% 
  select(variable, depth, mean_crps:sd_log)


##################################################################################
# diff between flare forecast and climatology

# read in climatology and calculate mean scores
scores <- read.csv(file.path(lake_directory, 'scores/sunp/climatology_scores.csv'))

means <- scores %>% 
  filter(as.Date(time) %in% buoy_dates) %>% 
  group_by(variable, depth, year) %>% 
  mutate(mean_crps = mean(crps),
         mean_log = mean(logs)) %>% 
  distinct(variable, year, depth, .keep_all = TRUE) %>% 
  dplyr::select(variable, depth, year, mean_crps:mean_log)
means$variable <- factor(means$variable, levels = c('temperature', 'oxygen'), 
                         ordered = TRUE, labels = c('temperature (C)', 'oxygen (mg/L)'))

ggplot(means, aes(x = mean_crps, y = mean_log)) +
  geom_point()

ggplot(means, aes(x = as.factor(year), y = mean_crps, color = as.factor(year))) +
  geom_point(size = 3) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_grid(depth~variable, scales = 'free') +
  ggtitle('Climatology Performance') +
  xlab('Year') +
  ylab('Climatology CRPS') +
  labs(color = 'Year') +
  theme_bw()

ggplot(means, aes(x = as.factor(year), y = mean_crps, color = as.factor(year))) +
  geom_point(size = 3) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_grid(depth~variable, scales = 'free') +
  ggtitle('Climatology Performance') +
  xlab('Year') +
  ylab('Climatology CRPS') +
  labs(color = 'Year') +
  theme_bw()

means2 <- means %>% 
  select(variable:mean_crps) %>% 
  rename(Clim = mean_crps)

mean_skill_horizon_depth_year2 <- mean_skill_horizon_depth_year %>%
  select(year:mean_crps) %>% 
  rename(Flare = mean_crps)

fl_clim <- left_join(mean_skill_horizon_depth_year2, means2)
fl_clim <- fl_clim %>% 
  pivot_longer(c(Flare, Clim), names_to = 'model', values_to = 'crps')


ggplot(fl_clim, aes(x = horizon, y = crps, color = as.factor(year), linetype = as.factor(model))) +
  #geom_point(size = 1.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_shape_manual(values = c(1, 16)) +
  facet_grid(depth~variable) +
  theme_bw() +
  xlab('Year') +
  ylab('Mean CRPS (°C or mg/L)') +
  labs(color = 'Year',
       linetype = 'Model')

horizon_wide <- fl_clim %>% 
  filter(horizon %in% c(1, 35),
         model=="Flare") %>% 
  pivot_wider(names_from = horizon, values_from = crps)

diff_horizon <- plyr::ddply(horizon_wide, c("variable", "depth", "year"), function(x){
  diff <- x$`1` - x$`35`

})
diff_horizon

fl_clim %>% 
  group_by(variable, depth, year, model) %>% 
  mutate(mean = mean(crps)) %>% 
ggplot(aes(x = as.factor(year), y = mean, color = as.factor(year), shape = as.factor(model))) +
  geom_point(size = 3) +
  #geom_line(size = 1.2) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_shape_manual(values = c(1, 16)) +
  facet_grid(depth~variable) +
  theme_bw() +
  xlab('Year') +
  ylab('Mean CRPS (°C or mg/L)') +
  labs(color = 'Year',
       shape = 'Model')

diff_null <- mean_skill_horizon_depth_year
diff_null$diff_crps <- NA
diff_null$diff_log <- NA

for(i in 1:nrow(diff_null)){
  idx <- which(means$variable==diff_null$variable[i] 
               & means$depth==diff_null$depth[i]
               & means$year==diff_null$year[i])
  diff_null$diff_crps[i] <- means$mean_crps[idx] - diff_null$mean_crps[i]
  diff_null$diff_log[i] <- means$mean_log[idx] - diff_null$mean_log[i]
}

ggplot(diff_null, aes(x = horizon, y = mean_crps, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(depth~variable, scales = 'free')

t_c <- ggplot(diff_null[diff_null$variable=='temperature (C)',], aes(x = horizon, y = diff_crps, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~depth, ncol = 1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylab('Diff from Null (°C)') +
  ylim(-1.5, 1.5) +
  ggtitle('Temperature, CRPS') +
  labs(color = 'Year') +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  theme_bw()
  
o_c <- ggplot(diff_null[diff_null$variable=='oxygen (mg/L)',], aes(x = horizon, y = diff_crps, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~depth, ncol = 1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylab('Diff from Null (mg/L)') +
  #ylim(-1.5, 1.5) +
  ggtitle('Oxygen, CRPS') +
  labs(color = 'Year') +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  theme_bw()

t_l <- ggplot(diff_null[diff_null$variable=='temperature (C)',], aes(x = horizon, y = diff_log, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~depth, ncol = 1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylab('Diff from Null') +
  ggtitle('Temperature, IGN') +
 # ylim(-1.5, 1.5) +
  labs(color = 'Year') +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  theme_bw()

o_l <- ggplot(diff_null[diff_null$variable=='oxygen (mg/L)',], aes(x = horizon, y = diff_log, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~depth, ncol = 1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylab('Diff from Null') +
  ggtitle('Oxygen, IGN') +
  labs(color = 'Year') +
  # ylim(-1.5, 1.5) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  theme_bw()




ggarrange(t_l, t_c, 
          o_l, o_c,
          common.legend = TRUE)

ggarrange(t_l, t_c, 
          o_l, o_c,
          common.legend = TRUE,
          align = "v")

ggarrange(t_c, 
          o_c,
          common.legend = TRUE,
          align = "v")

mean_diff <- diff_null %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, depth, year) %>% 
  mutate(mean_diff_crps = mean(diff_crps, na.rm = TRUE),
         sd_diff_crps = sd(diff_crps, na.rm = TRUE)) %>% 
  distinct(variable, depth, .keep_all = TRUE) %>% 
  select(variable, depth, mean_diff_crps, sd_diff_crps)
  
ggplot(mean_diff, aes(x = as.factor(year), y = mean_diff_crps, color = as.factor(year))) +
  geom_point(size = 3) +
  #geom_jitter(size = 3) +
  #ylim(0.5, 0.9) +
  geom_errorbar(aes(ymin = mean_diff_crps - sd_diff_crps, ymax = mean_diff_crps + sd_diff_crps), width = .2) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_grid(depth~variable) +
  theme_bw() +
  xlab('Year') +
  ylab('Diff from null') +
  labs(color = 'Variable',
       shape = 'Depth')

