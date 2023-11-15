# read in forecasts and look at variance over time

library(arrow)
library(tidyverse)

lake_directory <- here::here()

folder <- file.path(lake_directory, 'forecasts/sunp/all_UC')
files <- list.files(path = folder, pattern = "*.csv.gz")
f <- read_csv(file.path(folder, files[1])) # start with second file because first is the spinup period

# some subsetting
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)
#horizons <- c(1, 7, 20, 35)

f <- f %>% 
  filter(variable %in% vars,
         depth %in% depths)  

# read in files
for (i in 2:length(files)) {
  print(i)
  temp <- read_csv(paste0(folder, "/", files[i]))
  temp <- temp %>% 
    filter(variable %in% vars,
           depth %in% depths)  
  f <- rbind(f, temp)
}


f <- f %>%
  mutate(horizon = difftime(as.POSIXct(datetime), as.POSIXct(reference_datetime), units = 'days')) %>% 
  filter(horizon > 0) 

var <- f %>% 
  group_by(depth, variable, horizon) %>% 
  mutate(variance = var(prediction),
         var_from_sd = sd(prediction)^2) %>% 
  distinct(depth, datetime, variable, horizon, .keep_all = TRUE) 

ggplot(var[var$depth==1,], aes(x = horizon, y = variance)) +
  geom_point() + 
  facet_wrap(~variable, scales = 'free_y') +
  ggtitle('1.0m, var across all members')
  

ggplot(var[var$depth==1,], aes(x = horizon, y = var_from_sd)) +
  geom_point() + 
  facet_wrap(~variable, scales = 'free_y') +
  ggtitle('1.0m, var across all members')

ggplot(var[var$depth==10,], aes(x = horizon, y = variance)) +
  geom_point() + 
  facet_wrap(~variable, scales = 'free_y') +
  ggtitle('10.0m, var across all members')


#########################################################################
# calculate from scores using HLW's method
# calculate variance from scores

pacman::p_load(dplyr,ggplot2, verification)
install.packages('verification')
library(verification)

lake_directory <- here::here()
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)

#read in all forecasts 
score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory, 'scores/sunp/all_UC'))
all_forecasts <- arrow::open_dataset(score_dir) |> collect() |>   
  filter(!is.na(observation), variable %in% vars,
         depth %in% depths,
         #reference_datetime < '2022-06-01' & reference_datetime < '2022-08-30',
         horizon > 0) 

#round depths up to nearest m 
all_forecasts$depth <- ceiling(all_forecasts$depth)


#forecast skill for each depth and horizon
forecast_skill_depth_horizon <-  plyr::ddply(all_forecasts, c("depth","horizon", "variable", "model_id"), function(x) {
  data.frame(
    RMSE = sqrt(mean((x$mean - x$observation)^2, na.rm = TRUE)),
    MAE = mean(abs(x$mean - x$observation), na.rm = TRUE),
    pbias = 100 * (sum(x$mean - x$observation, na.rm = TRUE) / sum(x$observation, na.rm = TRUE)),
    CRPS = verification::crps(x$observation, as.matrix(x[, c(7,9)]))$CRPS,
    variance = (mean(x$sd))^2,
    sd = mean(x$sd)
  )
}, .progress = plyr::progress_text(), .parallel = FALSE) 


#rename depth facets
depths <- c("1m","10m")
names(depths) <- c("1","10")

ggplot(forecast_skill_depth_horizon[forecast_skill_depth_horizon$depth==1,], aes(x = horizon, y = variance)) +
  geom_point() +
  geom_line() +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') +
  ggtitle('variance from scores (sd^2)')

ggplot(forecast_skill_depth_horizon[forecast_skill_depth_horizon$depth==1,], aes(x = horizon, y = sd)) +
  geom_point() +
  geom_line() +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') +
  ggtitle('variance from scores (sd^2)')


###################################################################################################
# check forecast parquet files
#read in all forecasts 
score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory, 'forecasts/parquet'))
all_forecasts <- arrow::open_dataset(score_dir) |> collect() |>   
  filter(!is.na(observation), variable %in% vars,
         depth %in% depths,
         model_id=='all_UC',
         #reference_datetime < '2022-06-01' & reference_datetime < '2022-08-30',
         horizon > 0) 

#round depths up to nearest m 
all_forecasts$depth <- ceiling(all_forecasts$depth)

# some subsetting
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)
#horizons <- c(1, 7, 20, 35)

f <- f %>% 
  filter(variable %in% vars,
         depth %in% depths)  

# read in files
for (i in 2:length(files)) {
  print(i)
  temp <- read_csv(paste0(folder, "/", files[i]))
  temp <- temp %>% 
    filter(variable %in% vars,
           depth %in% depths)  
  f <- rbind(f, temp)
}


f <- f %>%
  mutate(horizon = difftime(as.POSIXct(datetime), as.POSIXct(reference_datetime), units = 'days')) %>% 
  filter(horizon > 0) 

var <- f %>% 
  group_by(depth, variable, horizon) %>% 
  mutate(variance = var(prediction),
         var_from_sd = sd(prediction)^2) %>% 
  distinct(depth, datetime, variable, horizon, .keep_all = TRUE) 

ggplot(var[var$depth==1,], aes(x = horizon, y = variance)) +
  geom_point() + 
  facet_wrap(~variable, scales = 'free_y') +
  ggtitle('1.0m, var across all members')


ggplot(var[var$depth==1,], aes(x = horizon, y = var_from_sd)) +
  geom_point() + 
  facet_wrap(~variable, scales = 'free_y') +
  ggtitle('1.0m, var across all members')

ggplot(var[var$depth==10,], aes(x = horizon, y = variance)) +
  geom_point() + 
  facet_wrap(~variable, scales = 'free_y') +
  ggtitle('10.0m, var across all members')

