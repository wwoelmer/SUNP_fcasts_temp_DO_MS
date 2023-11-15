# check the 1day_DA_1fcast parameter evolution
library(tidyverse)
lake_directory <- here::here()

forecast_folder <- file.path(lake_directory, 'forecasts/1DA_1fcast_all_files_sd/all_UC')
files1 <- list.files(path = forecast_folder, pattern = "*.csv.gz")
fcast <- read.csv(paste0(forecast_folder, "/", files1[1]))
fcast <- fcast %>% 
  filter(variable_type=="parameter")

for(i in 2:36){
  print(i)
  temp <- read.csv(paste0(forecast_folder, "/", files1[i]))
  temp <- temp %>% 
    filter(variable_type=="parameter")
  fcast <- rbind(fcast, temp)
}


ggplot(fcast, aes(x = as.Date(datetime), y = prediction)) +
  geom_line(aes(group = parameter)) +
  facet_wrap(~variable, scales = 'free_y')

summ <- fcast %>% 
  group_by(variable, datetime) %>% 
  mutate(mean = mean(prediction),
         sd = sd(prediction)) %>% 
  distinct(variable, datetime, .keep_all = TRUE)

ggplot(summ, aes(x = as.Date(datetime), y = mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean-sd, ymax = mean +sd)) +
  facet_wrap(~variable, scales = 'free_y')
