# compare forecast to observed
# compare UC bounds from each UC source to observations:
#   are we falling within the bounds for all sources?

library(arrow)
library(tidyverse)
lake_directory <- here::here()

folders <- c('all_UC', 'initial_condition', 'observation', 'parameter', 'process', 'weather')
score_folder <- file.path(lake_directory, 'scores/sunp', folders[1])
files1 <- list.files(path = score_folder, pattern = "*.parquet")
f <- read_parquet(file.path(score_folder, files1[2])) # start with second file because first is the spinup period

# some subsetting
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)
#horizons <- c(1, 7, 20, 35)

f <- f %>% 
  separate(site_id, into = c("site", "depth"), sep = "-") %>% 
  filter(variable %in% vars,
         depth %in% depths)  

ggplot(f, aes(x = as.Date(datetime), y = observation)) +
  geom_point(aes(color = horizon)) +
  geom_line(aes(y = mean)) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free')

ggplot(f, aes(x = horizon, y = observation)) +
  geom_point() +
  #xlim(0, 2) +
  geom_ribbon(aes(ymin = quantile02.5, ymax = quantile97.5), alpha = 0.2) +
  geom_line(aes(y = mean)) +
  geom_vline(xintercept = 0) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free')

starting_index <- 1
for(j in starting_index:length(folders)){
  score_folder <- file.path(lake_directory, 'scores/sunp', folders[j])
  print(folders[j])

  files1 <- list.files(path = score_folder, pattern = "*.parquet")
  dataset <- read_parquet(file.path(score_folder, files1[2]))
  
  dataset <- dataset %>% 
    separate(site_id, into = c("site", "depth"), sep = "-") %>% 
    filter(variable %in% vars,
           depth %in% depths)  
  
  # read in files
  for (i in 3:length(files1)) {
    print(i)
    temp <- read_parquet(paste0(score_folder, "/", files1[i]))
    temp <- temp %>% 
      separate(site_id, into = c("site", "depth"), sep = "-") %>% 
      filter(variable %in% vars,
             depth %in% depths)  
    dataset <- rbind(dataset, temp)
  }
  
  f <- rbind(f, dataset)
  
}

#write.csv(f, file.path(lake_directory, 'all_obs_fcasts.csv'), row.names = FALSE)

#f <- read.csv(file.path(lake_directory, 'all_obs_fcasts.csv'))

##########################################################################
# reliability diagrams
#rd <- f %>% 
#  select(reference_datetime, datetime, depth, model_id, variable, horizon,
#         observation, mean, sd)

#breaks <- seq(0, 1, by = 0.10)

# 10% CI, 0.55, 0.45

#for(i in 1:nrow(rd)){
#  for(j in 2:length(breaks)){
#    # subset to forecasts within each break range of the distribution
#    prob_above <- qnorm(breaks[j], rd$mean[i], rd$sd[i]) 
#    if(prob){
      # 1 if obs in in CI, 0 if not, count total number in
      
#    }
#  }
#}


##########################################################################
library(scoringRules)
horizons <- seq(1, 35, by = 1)
# get rid of instances where sd = 0 (spinup?)
f <- f %>% 
  filter(sd > 0) %>% 
#  filter(horizon > 0) %>% 
  filter(horizon %in% horizons) %>% 
  mutate(obs_mgL_C = ifelse(variable=='oxygen', observation*32/1000, observation),
         mean_mgL_C = ifelse(variable=='oxygen', mean*32/1000, mean),
         crps2 = crps(observation, family = "normal", mean = mean, sd = sd),
         crps2_mgL = ifelse(variable=='oxygen', crps2*32/1000, crps2))

ggplot(f[f$model_id=='all_UC',], aes(x = horizon, y = mean)) +
  geom_line(aes(col = as.factor(datetime))) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free')

ggplot(f[f$model_id=='all_UC' & f$horizon<3,], aes(x = horizon, y = mean)) +
  geom_line(aes(col = as.factor(datetime))) +
  geom_point(aes(x = horizon, y = observation, col = as.factor(datetime))) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free')

ggplot(f[f$model_id=='all_UC',]) +
  geom_point(aes(x = horizon, y = mean - observation, col = as.factor(reference_datetime))) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free')


crps_mean <- f %>% 
  filter(model_id=='all_UC') %>% 
  group_by(horizon, variable, depth) %>% 
  mutate(crps_mean = mean(crps2)) %>% 
  distinct(depth, horizon, variable, .keep_all = TRUE)

ggplot(crps_mean, aes(x = horizon, y = crps_mean)) +
  geom_point() +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free')

ggplot(f[f$model_id!='all_UC',], aes(x = as.Date(datetime), y = crps2_mgL)) +
  geom_point(aes(color = model_id)) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free')

ggplot(f[f$model_id=='all_UC',], aes(x = as.Date(datetime), y = crps2_mgL)) +
  geom_point(aes(color = as.factor(horizon))) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') +
  ggtitle('normal run (all UCs on)')

ggplot(f[f$model_id!='all_UC',], aes(x = as.Date(datetime), y = crps2_mgL)) +
  geom_point(aes(color = model_id, shape = as.factor(horizon))) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') 

ggplot(f[f$model_id=='all_UC',], aes(x = as.Date(datetime), y = obs_mgL_C)) +
  geom_point(aes(color = as.factor(horizon))) +
  geom_line(aes(y = mean_mgL_C)) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') +
  ggtitle('normal run (all UCs on)')


ggplot(f[f$variable=='oxygen',], aes(x = as.Date(datetime), y = mean_mgL_C)) +
  geom_point(aes(color = model_id)) +
  geom_line(aes(y = obs_mgL_C)) +
  facet_grid(cols = vars(model_id), rows = vars(depth), scale = 'free')

ggplot(f[f$variable=='temperature',], aes(x = as.Date(datetime), y = mean_mgL_C)) +
  geom_point(aes(color = model_id)) +
  geom_line(aes(y = obs_mgL_C)) +
  facet_grid(cols = vars(model_id), rows = vars(depth), scale = 'free')

# UC types with observations, 7 day horizon
ggplot(f[f$variable=='oxygen' & f$horizon==7,], aes(x = as.Date(datetime), y = mean_mgL_C)) +
  geom_line(aes(color = model_id)) +
  geom_ribbon(aes(ymax = quantile97.5*32/1000, ymin = quantile02.5*32/1000, fill = model_id)) +
  geom_point(aes(y = obs_mgL_C)) +
  facet_grid(cols = vars(model_id), rows = vars(depth), scale = 'free')

ggplot(f[f$variable=='temperature' & f$horizon==7,], aes(x = as.Date(datetime), y = mean_mgL_C)) +
  geom_ribbon(aes(ymax = quantile97.5, ymin = quantile02.5, fill = model_id)) +
  geom_line(aes(color = "black")) +
  geom_point(aes(y = obs_mgL_C)) +
  facet_grid(cols = vars(model_id), rows = vars(depth), scale = 'free')

# UC types with observations, 21 day horizon
ggplot(f[f$variable=='oxygen' & f$horizon==21,], aes(x = as.Date(datetime), y = mean_mgL_C)) +
  geom_line(aes(color = "black")) +
  geom_ribbon(aes(ymax = quantile97.5*32/1000, ymin = quantile02.5*32/1000, fill = model_id)) +
  geom_point(aes(y = obs_mgL_C)) +
  facet_grid(cols = vars(model_id), rows = vars(depth), scale = 'free')

ggplot(f[f$variable=='temperature' & f$horizon==21,], aes(x = as.Date(datetime), y = mean_mgL_C)) +
  geom_line(aes(color = model_id)) +
  geom_ribbon(aes(ymax = quantile97.5, ymin = quantile02.5, fill = model_id)) +
  geom_point(aes(y = obs_mgL_C)) +
  facet_grid(cols = vars(model_id), rows = vars(depth), scale = 'free')
