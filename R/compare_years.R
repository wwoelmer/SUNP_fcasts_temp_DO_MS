# analyze forecast skill from normal forecast run across year

library(tidyverse)
#install.packages('ggpubr')
library(ggpubr)

lake_directory <- here::here()

folders <- c('UC_analysis_2021/all_UC', 'all_UC')
forecast_folder <- file.path(lake_directory, 'forecasts/sunp/', folders[1])
files1 <- list.files(path = forecast_folder, pattern = "*.csv.gz")
fcast <- read.csv(paste0(forecast_folder, "/", files1[2]))

# some subsetting
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)
horizons <- c(1, 7, 35)
fcast <- fcast %>% 
  filter(variable %in% vars,
         depth %in% depths) %>% 
  mutate(horizon = difftime(as.POSIXct(datetime), as.POSIXct(reference_datetime), units = 'days')) %>% 
  filter(horizon %in% horizons) 

for(j in 1:length(folders)){
  forecast_folder <- file.path(lake_directory, 'forecasts/sunp', folders[j])
  print(paste0("folder: ", folders[j]))
  myfiles <- list.files(path = forecast_folder, pattern = "*.csv.gz")
  dataset <- read.csv(paste0(forecast_folder, "/", myfiles[2]))
  
  dataset <- dataset %>% 
    filter(variable %in% vars,
           depth %in% depths) %>% 
    mutate(horizon = difftime(as.POSIXct(datetime), as.POSIXct(reference_datetime), units = 'days')) %>% 
    filter(horizon %in% horizons) 
  
  # read in files
  for (i in 3:length(myfiles)) {
    print(i)
    temp <- read.csv(paste0(forecast_folder, "/", myfiles[i]))
    temp <- temp %>% 
      filter(variable %in% vars,
             forecast > 0,
             depth %in% depths) %>% 
      mutate(horizon = difftime(as.POSIXct(datetime), as.POSIXct(reference_datetime), units = 'days')) %>% 
      filter(horizon %in% horizons)
    dataset <- rbind(dataset, temp)
  }
  
  
  fcast <- rbind(fcast, dataset)
  
}

####################################################################################
# plot year to year differences

fcast <- fcast %>% 
  mutate(doy = yday(datetime),
         year = year(datetime)) %>% 
  select(-c(family, site_id, model_id, pub_time)) %>% 
  select()

ggplot(fcast, aes(x = as.factor(year), y = prediction, fill = as.factor(year))) +
  geom_boxplot() +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') 

ggplot(fcast[fcast$doy==240 & fcast$horizon==7,], aes(x = doy, y = prediction, fill = as.factor(year))) +
  geom_boxplot() +
  geom_point(data = scores[scores$doy==240 & scores$horizon==7,], aes(x = doy, y = observation, color = as.factor(year))) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free')   

ggplot(scores, aes(x = as.factor(year), y = observation, fill = as.factor(year))) +
  geom_boxplot() +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') 

####################################################################################
# read in scores
#2021
folders <- c('UC_analysis_2021/all_UC', 'all_UC')
score_folder <- file.path(lake_directory, 'scores/sunp', folders[1])
files21 <- list.files(path = score_folder, pattern = "*.parquet")
score21 <- read_parquet(paste0(score_folder, "/", files21[2]))

# some subsetting
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)

score21 <- score21 %>% 
  filter(variable %in% vars,
         depth %in% depths)  

# read in files
for (i in 3:length(files21)) {
  print(i)
  temp <- read_parquet(paste0(score_folder, "/", files21[i]))
  temp <- temp %>% 
    filter(variable %in% vars,
           depth %in% depths)  
  score21 <- rbind(score21, temp)
}

#2022
score_folder <- file.path(lake_directory, 'scores/sunp', folders[2])
files22 <- list.files(path = score_folder, pattern = "*.parquet")
score22 <- read_parquet(paste0(score_folder, "/", files22[2]))
score22 <- read_parquet(paste0(score_folder, "/", files22[129]))

# some subsetting
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)

score22 <- score22 %>% 
  filter(variable %in% vars,
         depth %in% depths)  

# read in files
for (i in 3:length(files22)) {
  print(i)
  temp <- read_parquet(paste0(score_folder, "/", files22[i]))
  temp <- temp %>% 
    filter(variable %in% vars,
           depth %in% depths)  
  score22 <- rbind(score22, temp)
}


scores <- full_join(score21, score22) %>% 
  mutate(doy = yday(datetime),
         year = year(datetime)) %>% 
  select(-c(family, site_id, model_id)) %>% 
  select(datetime, doy, variable, depth, horizon, mean, sd, crps, observation, everything())

ggplot(scores[scores$horizon==7,], aes(x = doy, y = crps, color = as.factor(year))) +
  geom_line() +
  xlim(220, 300) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') 
  
ggplot(scores[scores$horizon==7,], aes(x = doy, y = mean, color = as.factor(year))) +
  geom_line() +
  geom_point(aes(x = doy, y= observation, color = 'obs'), size = 0.2) +
#  geom_ribbon(aes(ymin = quantile02.5, ymax = quantile97.5, alpha = 0.4)) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') 

ggplot(scores[scores$horizon==7,], aes(x = doy, y = mean, color = as.factor(year))) +
  geom_point() +
  geom_errorbar(aes(ymin = quantile02.5, ymax = quantile97.5, color = as.factor(year))) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') 


#####################################################################################
# plot just one day's forecast across horizons
fcast_date <- as.POSIXct('2022-06-08')
folders
fcast <- read.csv(file.path(lake_directory, 'forecasts/sunp', folders[1], paste0("sunp-", fcast_date, "-",  folders[1], ".csv.gz")))
for(m in 2:length(folders)){
  temp <- read.csv(file.path(lake_directory, 'forecasts/sunp', folders[m], paste0("sunp-", fcast_date, "-",  folders[m], ".csv.gz")))
  fcast <- rbind(fcast, temp)
  
}

fcast <- fcast %>% 
  filter(variable %in% vars,
         depth %in% depths,
         forecast > 0) %>% 
  mutate(horizon = as.POSIXct(datetime) - as.POSIXct(reference_datetime)) 

fcast$model_id <- factor(fcast$model_id, levels = c("initial_condition",
                                                    "observation",
                                                    "parameter",
                                                    "process",
                                                    "weather"))

area <- fcast %>% 
  group_by(model_id, variable, depth, horizon) %>% 
  mutate(ymax = max(prediction),
         ymin = min(prediction),
         model_mean = mean(prediction)) %>%
  group_by(variable, depth, horizon) %>% 
  mutate(mean = mean(prediction)) %>% 
  select(reference_datetime, datetime,
         depth, model_id, variable, horizon,
         mean, model_mean, ymax, ymin) %>% 
  distinct(depth, model_id, variable, horizon, .keep_all = TRUE) %>% 
  mutate(diff = ymax - ymin)

# define plotting order of ribbons
area$ord <- NA
for(i in 1:nrow(area)){
  if(area$model_id[i]=="initial_condition"){
    area$ord[i] <- 1
  }
  if(area$model_id[i]=="observation"){
    area$ord[i] <- 2
  }
  if(area$model_id[i]=="parameter"){
    area$ord[i] <- 3
  }
  if(area$model_id[i]=="process"){
    area$ord[i] <- 4
  }
  if(area$model_id[i]=="weather"){
    area$ord[i] <- 5
  }
}



ggplot(area[area$depth==1,], aes(x = as.Date(datetime), y = mean)) +
  geom_line() +
  geom_line(aes(y = model_mean, color = model_id)) +
  facet_wrap(~variable, scales = 'free_y') 


ggplot(area[area$depth==1,], aes(x = as.Date(datetime), y = mean, fill =fct_rev(model_id))) +
  geom_ribbon(aes(position = "stack", ymin = ymin, ymax = ymax, alpha = 0.5)) +
  geom_line(aes(x = as.Date(datetime), y = mean)) +
  facet_wrap(~variable, scales = 'free_y') +
  ggtitle('1.0m')

ggplot(area[area$depth==10,], aes(x = as.Date(datetime), y = mean, fill =fct_rev(model_id))) +
  geom_ribbon(aes(position = "stack", ymin = ymin, ymax = ymax, alpha = 0.5)) +
  geom_line(aes(x = as.Date(datetime), y = mean)) +
  facet_wrap(~variable, scales = 'free_y') +
  ggtitle('10.0m')


ggplot(fcast[fcast$depth==1,], aes(x = as.Date(datetime), y = prediction, color = model_id)) +
  geom_line() +
  facet_wrap(~variable, scales = 'free_y') +
  ggtitle('1.0m')

ggplot(area[area$variable=='temperature',], aes(x = as.Date(datetime), y = model_mean, color = model_id)) +
  geom_line() +
  geom_ribbon(aes(fill = model_id, ymin = ymin, ymax = ymax, alpha = 0.5)) +
  facet_grid(cols = vars(model_id), rows = vars(depth), scales = 'free')+
  ggtitle('temperature')

ggplot(area[area$variable=='oxygen',], aes(x = as.Date(datetime), y = model_mean, color = model_id)) +
  geom_line() +
  geom_ribbon(aes(fill = model_id, ymin = ymin, ymax = ymax, alpha = 0.5)) +
  facet_grid(cols = vars(model_id), rows = vars(depth), scales = 'free') +
  ggtitle('oxygen')


##############################################################################################
# visualize the normal run (all_UC)
forecast_folder <- file.path(lake_directory, 'forecasts/sunp/all_UC')

# the year's forecasts with all uncertainties 'on' in the forecast output
myfiles <- list.files(path = forecast_folder, pattern = "*.csv.gz")
dataset <- read.csv(paste0(forecast_folder, "/", myfiles[3]))

# some subsetting
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)
dataset <- dataset %>% 
  filter(variable %in% vars,
         forecast > 0,
         depth %in% depths) %>% 
  mutate(horizon = as.POSIXct(datetime) - as.POSIXct(reference_datetime)) %>% 
  filter(horizon==1)

# read in files
for (i in 2:length(myfiles)) {
  print(i)
  temp <- read.csv(paste0(forecast_folder, "/", myfiles[i]))
  temp <- temp %>% 
    filter(variable %in% vars,
           forecast > 0,
           depth %in% depths) %>% 
    mutate(horizon = as.POSIXct(datetime) - as.POSIXct(reference_datetime)) %>% 
    filter(horizon==1)
  dataset <- rbind(dataset, temp)
}

dataset_sum <- dataset %>% 
  group_by(variable, depth, datetime) %>% 
  mutate(variance = var(prediction)) %>% 
  distinct(variable, depth, datetime, .keep_all = TRUE)

dataset_sum$depth <- as.factor(dataset_sum$depth)

ggplot(dataset_sum, aes(x = as.POSIXct(datetime), y = variance, group = depth)) +
  geom_line(aes(color = depth)) +
  facet_wrap(~variable, scales = 'free_y') +
  xlab('Date')

