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
  score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp", folders[i]))
  
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
         as.Date(datetime) %in% buoy_dates) %>% 
  select(model_id, reference_datetime, datetime, horizon, depth, variable, everything()) 

# convert oxy crps and obs to mg/L
sc <- sc %>% 
  mutate(crps = ifelse(variable=='temperature', crps, (crps*32/1000)),
         mean = ifelse(variable=='temperature', mean, (mean*32/1000)),
         observation = ifelse(variable=='temperature', observation, (observation*32/1000)))

sc$variable <- factor(sc$variable, levels = c('temperature', 'oxygen'), 
                      ordered = TRUE, labels = c('temperature (C)', 'oxygen (mg/L)'))


mean_skill <- sc %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, horizon, depth, year) %>% 
  mutate(mean_crps = mean(crps, na.rm = TRUE),
         mean_log = mean(logs, na.rm = TRUE),
         mean_rmse = mean(rmse, na.rm = TRUE)) %>% 
  distinct(variable, horizon, depth, .keep_all = TRUE) %>% 
  select(variable, horizon, depth, mean_crps, mean_log, mean_rmse)

out <- plyr::ddply(mean_skill, c("variable", "depth", "year"), function(x){
  
  slope <- coef(lm(x$mean_crps ~ x$horizon))[2]
  return(slope)
})
colnames(out) <- c('variable', 'depth', 'year', 'slope')
out$y <- c(1.4, 1.2, 1.7, 1.5, 1.4, 1.2, 1.8, 1.6)
out$x <- rep(9, 8)  

#################################################################################
# plot rmse over horizon
rmse <- sc %>% 
  group_by(depth, variable, horizon, year) %>% 
  mutate(rmse = rmse(observation, mean)) %>% 
  distinct(depth, variable, horizon, year, .keep_all = TRUE) %>% 
  select(year, depth, variable, horizon, rmse)

o <- ggplot(rmse[rmse$variable=='oxygen',], aes(x = horizon, y = rmse, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
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
o

t <- ggplot(rmse[rmse$variable=='temperature',], aes(x = horizon, y = rmse, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~depth) +
  xlab('horizon (days into future)') +
  ylab('RMSE') +
  ggtitle('Temperature') +
  labs(color = 'Year') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
t

ggarrange(t, o, common.legend = TRUE)

##################################################################################
# read in climatology and calculate mean scores
scores <- read.csv('./scores/sunp/climatology_scores.csv')

means <- scores %>% 
  filter(as.Date(time) %in% buoy_dates) %>% 
  group_by(variable, depth, year) %>% 
  mutate(mean_crps = mean(crps),
         mean_log = mean(logs)) %>% 
  distinct(variable, year, depth, .keep_all = TRUE) %>% 
  dplyr::select(variable, depth, year, mean_crps, mean_log)
means$variable <- factor(means$variable, levels = c('temperature', 'oxygen'), 
                         ordered = TRUE, labels = c('temperature (C)', 'oxygen (mg/L)'))

ggplot(means, aes(x = mean_crps, y = mean_log)) +
  geom_point()

################################################################################
# log score over horizon figures
o <- ggplot(mean_skill[mean_skill$variable=='oxygen (mg/L)',], aes(x = horizon, y = mean_log, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~depth) +
  xlab('horizon (days into future)') +
  ylab('log score') +
  ggtitle('Oxygen') +
  labs(color = 'Year') +
  #geom_hline(data = means[means$variable=='oxygen (mg/L)',], 
  #           aes(yintercept = mean_log, color = as.factor(year)),
  #           linetype = 'dashed') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
o

t <- ggplot(mean_skill[mean_skill$variable=='temperature (C)',], aes(x = horizon, y = mean_log, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~depth) +
  xlab('horizon (days into future)') +
  ylab('log score') +
  ggtitle('Temperature') +
  labs(color = 'Year') +
  geom_hline(data = means[means$variable=='temperature (C)',],  
             aes(yintercept = mean_log, color = as.factor(year)), 
             linetype = 'dashed') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
t

ggarrange(t, o, common.legend = TRUE)
###########################################################################
## CRPS
ggplot(mean_skill, aes(x = horizon, y = mean_crps, color = as.factor(year))) +
  geom_line() +
  stat_smooth(method = "lm") +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  geom_text(data = out, aes(label = paste0('slope = ', round(slope, 3)), x= x, y = y)) +
  xlab('horizon (days into future)') +
  ylab('CRPS') +
  labs(color = 'Year') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))


ggplot(mean_skill, aes(x = horizon, y = mean_crps, color = as.factor(year))) +
  geom_line() +
  stat_smooth(method = "lm") +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  geom_text(data = out, aes(label = paste0('slope = ', round(slope, 3)), x= x, y = y)) +
  geom_hline(data = means, aes(yintercept = mean_crps, color = as.factor(year)), linetype = 'dashed') +
  xlab('horizon (days into future)') +
  ylab('CRPS') +
  labs(color = 'Year') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
