library(tidyverse)
#install.packages('ggpubr')
library(ggpubr)

lake_directory <- here::here()

folders <- c('initial_condition', 'observation', 'parameter', 'process', 'weather')
forecast_folder <- file.path(lake_directory, 'forecasts/sunp/', folders[1])
files1 <- list.files(path = forecast_folder, pattern = "*.csv.gz")
uncert <- read.csv(paste0(forecast_folder, "/", files1[2]))

# some subsetting
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)
horizons <- c(1:35)
uncert <- uncert %>% 
  filter(variable %in% vars,
         forecast > 0,
         depth %in% depths) %>% 
  mutate(horizon = difftime(as.POSIXct(datetime), as.POSIXct(reference_datetime), units = 'days')) %>% 
  filter(horizon %in% horizons) 
  
starting_index <- 1
# j <- starting_index
for(j in starting_index:length(folders)){
  forecast_folder <- file.path(lake_directory, 'forecasts/sunp', folders[j])
  print(folders[j])
  # the year's forecasts with all uncertainties 'on' in the forecast output
  myfiles <- list.files(path = forecast_folder, pattern = "*.csv.gz")
  dataset <- read.csv(paste0(forecast_folder, "/", myfiles[2]))
  
  dataset <- dataset %>% 
    filter(variable %in% vars,
           forecast > 0,
           depth %in% depths) %>% 
    mutate(horizon = as.POSIXct(datetime) - as.POSIXct(reference_datetime)) %>% 
    filter(horizon %in% horizons) 
  
  # read in files
  for (i in 3:length(myfiles)) {
    print(i)
    temp <- read.csv(paste0(forecast_folder, "/", myfiles[i]))
    temp <- temp %>% 
      filter(variable %in% vars,
             forecast > 0,
             depth %in% depths) %>% 
      mutate(horizon = as.POSIXct(datetime) - as.POSIXct(reference_datetime)) %>% 
      filter(horizon %in% horizons)
    dataset <- rbind(dataset, temp)
  }
  
  dataset_sum <- dataset %>% 
    group_by(variable, depth, datetime) %>% 
    mutate(variance = var(prediction)) %>% 
    mutate(UC_type = folders[j]) %>% 
    distinct(variable, depth, datetime, .keep_all = TRUE)
  
  dataset_sum$depth <- as.factor(dataset_sum$depth)
  
  print(
    ggplot(dataset_sum, aes(x = as.POSIXct(datetime), y = variance, group = depth)) +
    ggtitle(dataset_sum$UC_type) +
    geom_line(aes(color = depth)) +
    facet_wrap(~variable, scales = 'free_y') +
    xlab('Date')
  )
  
  uncert <- rbind(uncert, dataset)
  
}

write.csv(uncert, file.path(lake_directory, 'all_uncertainty.csv'), row.names = FALSE)

##############################################################
# now look at all forecasts over time at different horizons
uncert <- read.csv(file.path(lake_directory, 'all_uncertainty.csv'))

uncert_sum <- uncert %>% 
  select(datetime, depth, model_id, variable, parameter, prediction, horizon) %>% 
  group_by(model_id, variable, depth, datetime, horizon) %>% 
  mutate(variance = var(prediction)) %>% 
  mutate(mean_pred = mean(prediction)) %>% 
  distinct(variable, depth, datetime, horizon, .keep_all = TRUE) %>% 
  select(-prediction, -parameter)

uncert_prop <- uncert_sum %>% 
  group_by(variable, depth, datetime, horizon) %>% 
  mutate(total_var = sum(variance)) %>% 
  mutate(var_prop = variance/total_var)

# aggregate over each horizon 
# create dataframe to write into inside the loop (nrow = # of horizons, ncol = # of UC modes + 1)

uncert_mean <- uncert_prop %>% 
  group_by(horizon, variable, depth, model_id) %>% 
  mutate(mean_prop = mean(var_prop)) %>% 
  distinct(horizon, variable, depth, model_id, .keep_all = TRUE)

################
# make some figs
# stacked bar plot over all horizons
ggplot(uncert_mean, aes(x = horizon, y = mean_prop, fill = model_id)) +
  geom_bar(stat = 'identity', position= 'stack') +
  facet_grid(cols = vars(variable), rows = vars(depth)) 

# subset down to a few horizons
horizon_sub <- c(1, 7, 20, 35)
uncert_prop2 <- uncert_prop %>% 
  filter(horizon %in% horizon_sub)

ggplot(uncert_prop2[uncert_prop2$depth==1,], 
       aes(x = as.POSIXct(datetime), y = var_prop, fill = model_id)) +
  geom_area() +
  facet_grid(cols = vars(variable), rows = vars(horizon)) +
  ggtitle("1.0m")

ggplot(uncert_prop2[uncert_prop2$depth==10,], 
       aes(x = as.POSIXct(datetime), y = var_prop, fill = model_id)) +
  geom_area() +
  facet_grid(cols = vars(variable), rows = vars(horizon)) +
  ggtitle("10.0m")

t <- ggplot(uncert_prop2[uncert_prop2$variable=='temperature',], 
            aes(x = as.POSIXct(datetime), y = variance, group = model_id, color = model_id)) +
  geom_line() +
  facet_grid(cols = vars(depth), rows = vars(horizon), scales = 'free') +
  ggtitle('temperature') +
  xlab('Date')

o <- ggplot(uncert_prop2[uncert_prop2$variable=='oxygen',], 
            aes(x = as.POSIXct(datetime), y = variance, group = model_id, color = model_id)) +
  geom_line() +
  facet_grid(cols = vars(depth), rows = vars(horizon), scales = 'free') +
  ggtitle('oxygen') +
  xlab('Date')

ggarrange(o, t, common.legend = TRUE)



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

