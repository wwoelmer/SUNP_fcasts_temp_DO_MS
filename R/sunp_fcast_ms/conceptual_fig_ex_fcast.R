### extract example forecasts of each archetype (accuracy and precision axes)
library(Metrics)
library(tidyverse)

lake_directory <- here::here()

# some subsetting variables
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)
horizons <- c(1:35)
folder <- 'SUNP_fsed_deep_DA'
sim_name <- 'all_UC_fsed_deep_DA'
site_id <- 'sunp'

########################################################################
# read in the scores 
score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp", folder, sim_name))

sc <- arrow::open_dataset(score_dir) |> 
  filter(variable %in% vars,
         depth %in% depths) %>% 
  collect() 

for(i in 1:length(folder)){
  score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp", folder, sim_name))
  
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
  filter(horizon > 0,#) %>% 
         as.Date(reference_datetime) %in% buoy_dates) %>%
  select(model_id, reference_datetime, datetime, horizon, depth, variable, everything()) 

# convert oxy crps and obs to mg/L
sc <- sc %>% 
  mutate(crps = ifelse(variable=='temperature', crps, (crps*32/1000)),
         observation = ifelse(variable=='temperature', observation, (observation*32/1000)),
         mean = ifelse(variable=='temperature', mean, (mean*32/1000)),
         sd = ifelse(variable=='temperature', sd, (sd*32/1000)))


### calculate rmse
sc <- sc %>% 
  group_by(depth, variable, horizon, datetime) %>% 
  mutate(rmse = rmse(observation, mean)) 


##### look at just oxygen forecasts
sc2 <- sc %>% 
  filter(variable=='temperature') %>% 
  mutate(diff = abs(observation - mean)) %>% 
  select(reference_datetime, datetime:logs, rmse, mean, sd, diff, everything())

####################################################################################
density_fcast <- function(fdate, fhorizon, fdepth, fvariable, scores_df, title = NULL){
  fcast <- read.csv(file.path(lake_directory, 'forecasts/sunp', folder, sim_name, paste0('sunp-', fdate, "-", sim_name, '.csv.gz')))
  
  fcast <- fcast %>% 
    mutate(horizon = difftime(as.POSIXct(datetime), as.POSIXct(reference_datetime), units = 'days')) %>% 
    filter(depth==fdepth,
           variable==fvariable)
  
  obs <- scores_df %>% 
    filter(reference_datetime==paste0(fdate, " 00:00:00"),
           depth==fdepth,
           variable==fvariable,
           horizon==fhorizon)
  
  all_obs <- scores_df %>% 
    filter(reference_datetime==paste0(fdate, " 00:00:00"),
           depth==fdepth,
           variable==fvariable)
  
  convf <- 1
  
  if(fvariable=='oxygen'){
    convf <- 32/1000
  }
  
  a <- ggplot(all_obs, aes(x = as.POSIXct(datetime), y = mean)) +
    geom_line() +
    geom_ribbon(aes(ymin = quantile97.5*convf, ymax = quantile02.5*convf), alpha = 0.5) +
    geom_point(data = all_obs, aes(x = as.POSIXct(datetime), y = observation, color = '')) +
    geom_point(data = obs, aes(x = datetime, y = observation, color = 'Observation')) +
    scale_color_manual(values = c('black', 'red')) +
    ylim(0, 27) +
    #scale_x_date(breaks = date_breaks('1 week')) +
    labs(color = "", alpha = "") +
    ylab('Forecast') +
    xlab('Date') +
    theme_bw() +
    ggtitle(title)
  print(a)
  
  b <- fcast %>% 
    filter(horizon==fhorizon) %>% 
    ggplot(aes(x = prediction*convf, linetype = 'forecast')) +
    geom_density() +
    geom_point(data = obs, aes(x = observation, y = 0.01, color = 'Observation'), size = 3) +
    theme_bw() +
    scale_color_manual(values = c('red')) +
    xlim(5, 30) +
    xlab('Prediction (mg/L)') +
    ylab('Density')  +
    labs(color = "", linetype = "") +
    ggtitle(title)
  print(b)
  
}
####################################################################################
#low precision low accuracy 
density_fcast('2021-08-24', fhorizon = 24, fdepth = 10, fvariable = "temperature", scores_df = sc2, title = 'low precision, low accuracy')

#low precision high accuracy 
density_fcast('2022-08-14', fhorizon = 35, fdepth = 10, fvariable = "temperature", scores_df = sc2, title = 'low precision, high accuracy')
density_fcast(fdate = '2021-09-10', fhorizon = 20, fdepth = 1, fvariable = "temperature", scores_df = sc2, title = 'low precision, high accuracy')


# currently when saved as object, only the last figure is saved, need to figure out how to export both
# high precision, high accuracy
density_fcast(fdate = '2022-08-27', fhorizon = 1, fdepth = 1, fvariable = "temperature", scores_df = sc2, title = 'high precision, high accuracy')

# high precision, low accuracy
density_fcast(fdate = '2021-10-04', fhorizon = 1, fdepth = 10, fvariable = "temperature", scores_df = sc2, title = 'high precision, low accuracy')

