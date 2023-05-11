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
folders <- c('all_UC')

########################################################################
# read in the scores and calculate variance
score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp/UC_analysis_2021/start_06_30", folders[1]))

sc <- arrow::open_dataset(score_dir) |> 
  filter(variable %in% vars,
         depth %in% depths) %>% 
  collect() 

for(i in 1:length(folders)){
  score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp/UC_analysis_2021/start_06_30", folders[i]))
  
  temp <- arrow::open_dataset(score_dir) |> 
    filter(variable %in% vars,
           depth %in% depths) %>% 
    collect() 
  
  sc <- rbind(sc, temp)
  
}

# now read in 2022 data
for(i in 1:length(folders)){
  score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp/UC_analysis_2022", folders[i]))
  
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

sc$variable <- factor(sc$variable, levels = c('temperature', 'oxygen'), 
                      ordered = TRUE, labels = c('temperature (C)', 'oxygen (mg/L)'))

### calculate rmse
sc <- sc %>% 
  group_by(depth, variable, horizon, datetime) %>% 
  mutate(rmse = rmse(observation, mean)) 

#### aggregate across horizon
mean_skill <- sc %>% 
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

out <- plyr::ddply(mean_skill, c("variable", "depth", "year"), function(x){
  
  slope <- coef(lm(x$mean_crps ~ x$horizon))[2]
  return(slope)
})
colnames(out) <- c('variable', 'depth', 'year', 'slope')
out$y <- c(1.7, 1.5, 1.7, 1.5, 1.4, 1.2, 1.4, 1.2)
out$x <- rep(12, 8)  

#################################################################################
# plot rmse over horizon
rmse <- sc %>% 
  group_by(depth, variable, horizon, year) %>% 
  mutate(rmse = rmse(observation, mean)) %>% 
  distinct(depth, variable, horizon, year, .keep_all = TRUE) %>% 
  select(year, depth, variable, horizon, rmse)

o_r <- ggplot(mean_skill[mean_skill$variable=='oxygen (mg/L)',], aes(x = horizon, y = mean_rmse, color = as.factor(year))) +
  geom_line() +
  geom_ribbon(aes(ymax = mean_rmse + sd_rmse, ymin = mean_rmse - sd_rmse, 
                  col = as.factor(year),
                  fill = as.factor(year)),
              alpha = 0.5) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~depth) +
  xlab('horizon (days into future)') +
  ylab('RMSE') +
  ggtitle('Oxygen') +
  labs(color = 'Year') +
  #geom_hline(data = means[means$variable=='oxygen (mg/L)',], 
  #           aes(yintercept = mean_log, color = as.factor(year)),
  #           linetype = 'dashed') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
o_r

t_r <- ggplot(mean_skill[mean_skill$variable=='temperature (C)',], aes(x = horizon, y = mean_rmse, color = as.factor(year))) +
  geom_line() +
  geom_ribbon(aes(ymax = mean_rmse + sd_rmse, ymin = mean_rmse - sd_rmse, 
                  col = as.factor(year),
                  fill = as.factor(year)),
              alpha = 0.5) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~depth) +
  xlab('horizon (days into future)') +
  ylab('RMSE') +
  ggtitle('Temperature') +
  labs(color = 'Year') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
t_r

ggarrange(t_r, o_r, common.legend = TRUE)

##################################################################################
# read in climatology and calculate mean scores
scores <- read.csv('./scores/sunp/climatology_scores.csv')

means <- scores %>% 
  filter(as.Date(time) %in% buoy_dates) %>% 
  group_by(variable, depth, year) %>% 
  mutate(mean_crps = mean(crps),
         mean_log = mean(logs),
         mean_rmse = mean(rmse)) %>% 
  distinct(variable, year, depth, .keep_all = TRUE) %>% 
  dplyr::select(variable, depth, year, mean_crps:mean_rmse)
means$variable <- factor(means$variable, levels = c('temperature', 'oxygen'), 
                         ordered = TRUE, labels = c('temperature (C)', 'oxygen (mg/L)'))

ggplot(means, aes(x = mean_crps, y = mean_log)) +
  geom_point()
ggplot(means, aes(x = mean_crps, y = mean_rmse)) +
  geom_point()
ggplot(means, aes(x = mean_log, y = mean_rmse)) +
  geom_point()

################################################################################
# log score over horizon figures
o <- ggplot(mean_skill[mean_skill$variable=='oxygen (mg/L)',], aes(x = horizon, y = mean_log, color = as.factor(year))) +
  geom_line() +
  geom_ribbon(aes(ymax = mean_log + sd_log, ymin = mean_log - sd_log, 
                  col = as.factor(year),
                  fill = as.factor(year)),
              alpha = 0.5) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~depth) +
  xlab('horizon (days into future)') +
  ylab('IGN Score') +
  ylim(4, 7) +
  ggtitle('Oxygen') +
  labs(color = 'Year', fill = 'Year') +
 # geom_hline(data = means[means$variable=='oxygen (mg/L)',], 
  #           aes(yintercept = mean_log, color = as.factor(year)),
  #           linetype = 'dashed') +
  theme_bw()
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
o

t <- ggplot(mean_skill[mean_skill$variable=='temperature (C)',], aes(x = horizon, y = mean_log, color = as.factor(year))) +
  geom_line() +
  geom_ribbon(aes(ymax = mean_log + sd_log, ymin = mean_log - sd_log, 
                  col = as.factor(year),
                  fill = as.factor(year)),
              alpha = 0.5) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +  facet_wrap(~depth) +
  xlab('horizon (days into future)') +
  ylab('IGN Score') +
  ylim(0, 3) +
  ggtitle('Temperature') +
  labs(color = 'Year', fill = 'Year') +
  #geom_hline(data = means[means$variable=='temperature (C)',],  
  #           aes(yintercept = mean_log, color = as.factor(year)), 
  #           linetype = 'dashed') +
  theme_bw()
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
t

ggarrange(t, o, common.legend = TRUE)

### LSPA poster plot


###########################################################################
## CRPS
ggplot(mean_skill, aes(x = horizon, y = mean_crps, color = as.factor(year))) +
  geom_line() +
  stat_smooth(method = "lm") +
#  geom_ribbon(aes(ymax = mean_crps + sd_crps, ymin = mean_crps - sd_crps, 
#                  col = as.factor(year),
#                  fill = as.factor(year)),
#              alpha = 0.5) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +  
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  geom_text(data = out, aes(label = paste0('slope = ', round(slope, 3)), x= x, y = y)) +
  geom_hline(data = means, aes(yintercept = mean_crps, color = as.factor(year)), linetype = 'dashed') +
  xlab('horizon (days into future)') +
  ylab('CRPS') +
  labs(color = 'Year') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))


####

o_crps <- ggplot(mean_skill[mean_skill$variable=='oxygen (mg/L)',], aes(x = horizon, y = mean_crps, color = as.factor(year))) +
  geom_line() +
  #stat_smooth(method = "lm") +
  geom_ribbon(aes(ymax = mean_crps + sd_crps, ymin = mean_crps - sd_crps, 
                  col = as.factor(year),
                  fill = as.factor(year)),
              alpha = 0.5) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +   facet_wrap(~depth) +
  xlab('horizon (days into future)') +
  ylab('CRPS (mg/L)') +
  ggtitle('Oxygen (mg/L)') +
  labs(color = 'Year', fill = 'Year') +
  #ylim(0,1.5) +
  #geom_text(data = out[out$variable=='oxygen (mg/L)',], 
  #          aes(label = paste0('slope = ', round(slope, 3)),
  #              x= x, y = y), size = 3) +
  #geom_hline(data = means[means$variable=='oxygen (mg/L)',], 
  #           aes(yintercept = mean_crps, color = as.factor(year)),
  #           linetype = 'dashed') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
o_crps

t_crps <- ggplot(mean_skill[mean_skill$variable=='temperature (C)',], aes(x = horizon, y = mean_crps, color = as.factor(year))) +
  geom_line() +
  #stat_smooth(method = "lm") +
  geom_ribbon(aes(ymax = mean_crps + sd_crps, ymin = mean_crps - sd_crps, 
                  col = as.factor(year),
                  fill = as.factor(year)),
              alpha = 0.5) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) + 
  facet_wrap(~depth) +
  xlab('horizon (days into future)') +
  ylab('CRPS (°C)') +
  ggtitle('Temperature (°C)') +
  labs(color = 'Year', fill = 'Year') +
  #geom_text(data = out[out$variable=='temperature (C)',], 
  #          aes(label = paste0('slope = ', round(slope, 3)),
  #              x= x, y = y), size = 3) +
  #geom_hline(data = means[means$variable=='temperature (C)',], 
  #           aes(yintercept = mean_crps, color = as.factor(year)),
  #           linetype = 'dashed') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
t_crps

ggarrange(t_crps, o_crps, common.legend = TRUE)

ggarrange(t, t_crps, t_r,
          o, o_crps, o_r,
          common.legend = TRUE,
          align = 'v')

##################################################################################
# diff between flare forecast and climatology
diff_null <- mean_skill
diff_null$diff_crps <- NA
diff_null$diff_log <- NA
diff_null$diff_rmse <- NA

for(i in 1:nrow(diff_null)){
  idx <- which(means$variable==diff_null$variable[i] 
               & means$depth==diff_null$depth[i]
               & means$year==diff_null$year[i])
  diff_null$diff_crps[i] <- means$mean_crps[idx] - diff_null$mean_crps[i]
  diff_null$diff_log[i] <- means$mean_log[idx] - diff_null$mean_log[i]
  diff_null$diff_rmse[i] <- means$mean_rmse[idx] - diff_null$mean_rmse[i]
}

t_c <- ggplot(diff_null[diff_null$variable=='temperature (C)',], aes(x = horizon, y = diff_crps, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~depth) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylab('Difference from Null') +
  ylim(-1.5, 1.5) +
  ggtitle('Temperature, CRPS') +
  labs(color = 'Year') +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  theme_bw()
  
o_c <- ggplot(diff_null[diff_null$variable=='oxygen (mg/L)',], aes(x = horizon, y = diff_crps, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~depth) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylab('Difference from Null (mg/L)') +
  #ylim(-1.5, 1.5) +
  ggtitle('Oxygen, CRPS') +
  labs(color = 'Year') +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  theme_bw()

t_l <- ggplot(diff_null[diff_null$variable=='temperature (C)',], aes(x = horizon, y = diff_log, color = as.factor(year))) +
  geom_line() +
  facet_grid(~depth) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylab('Difference from Null') +
  ggtitle('Temperature, Log Score') +
 # ylim(-1.5, 1.5) +
  labs(color = 'Year') +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  theme_bw()

o_l <- ggplot(diff_null[diff_null$variable=='oxygen (mg/L)',], aes(x = horizon, y = diff_log, color = as.factor(year))) +
  geom_line() +
  facet_grid(~depth) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylab('Difference (mg/L)') +
  ggtitle('Oxygen, Log Score') +
  labs(color = 'Year') +
  # ylim(-1.5, 1.5) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  theme_bw()

t_r <- ggplot(diff_null[diff_null$variable=='temperature (C)',], aes(x = horizon, y = diff_rmse, color = as.factor(year))) +
  geom_line() +
  facet_grid(~depth) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylab('Difference from Null') +
  ggtitle('Temperature, RMSE') +
  # ylim(-1.5, 1.5) +
  labs(color = 'Year') +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  theme_bw()

o_r <- ggplot(diff_null[diff_null$variable=='oxygen (mg/L)',], aes(x = horizon, y = diff_rmse, color = as.factor(year))) +
  geom_line() +
  facet_grid(~depth) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylab('Difference (mg/L)') +
  ggtitle('Oxygen, RMSE') +
  labs(color = 'Year') +
  # ylim(-1.5, 1.5) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  theme_bw()

ggarrange(t_l, t_c, t_r, 
          o_l, o_c, o_r,
          common.legend = TRUE)

######################################################################################
### LSPA Poster

o_crps <- ggplot(mean_skill[mean_skill$variable=='oxygen (mg/L)',], aes(x = horizon, y = mean_crps, color = as.factor(year))) +
  geom_line() +
  #stat_smooth(method = "lm") +
  #  geom_ribbon(aes(ymax = mean_crps + sd_crps, ymin = mean_crps - sd_crps, 
  #                  col = as.factor(year),
  #                  fill = as.factor(year)),
  #              alpha = 0.5) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +   
  facet_wrap(~depth, ncol = 1) +
  xlab('horizon (days into future)') +
  ylab('Forecast Performance (mg/L)') +
  ggtitle('Oxygen (mg/L)') +
  labs(color = 'Year', fill = 'Year') +
  theme_bw()
o_crps

t_crps <- ggplot(mean_skill[mean_skill$variable=='temperature (C)',], aes(x = horizon, y = mean_crps, color = as.factor(year))) +
  geom_line() +
  #stat_smooth(method = "lm") +
  #  geom_ribbon(aes(ymax = mean_crps + sd_crps, ymin = mean_crps - sd_crps, 
  #                  col = as.factor(year),
  #                  fill = as.factor(year)),
  #              alpha = 0.5) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) + 
  facet_wrap(~depth, ncol = 1) +
  xlab('horizon (days into future)') +
  ylab('Forecast Performance (°C)') +
  ggtitle('Temperature (°C)') +
  labs(color = 'Year', fill = 'Year') +
  #geom_text(data = out[out$variable=='temperature (C)',], 
  #          aes(label = paste0('slope = ', round(slope, 3)),
  #              x= x, y = y), size = 3) +
  #geom_hline(data = means[means$variable=='temperature (C)',], 
  #           aes(yintercept = mean_crps, color = as.factor(year)),
  #           linetype = 'dashed') +
  theme_bw()
t_crps

ggarrange(t_crps, o_crps, common.legend = TRUE)
