library(tidyverse)
library(lubridate)
library(fable)
library(scoringRules)

options(dplyr.summarise.inform = FALSE)


##### 1. Random walk function from FO ####
forecast.RW  <- function(start, h= 36, depth_use) {
  
  # Work out when the forecast should start
  forecast_starts <- targets %>%
    dplyr::filter(!is.na(observed) & depth == depth_use, time < start)
  forecast_starts
  
  if (nrow(forecast_starts) !=0) {
    forecast_starts <- forecast_starts %>% 
      # Start the day after the most recent non-NA value
      dplyr::summarise(start_date = as.Date(max(time) + lubridate::days(1))) %>% # Date
      dplyr::mutate(h = (start - start_date) + h) %>% # Horizon value
      dplyr::ungroup()
    
    # Generate the RW model
    RW_model <- targets %>%
      mutate(time = as_date(time)) %>%
      # filter the targets data for the depth and start time
      dplyr::filter(depth == depth_use & time < start) %>%
      tsibble::as_tsibble(key = 'depth', index = 'time') %>%
      # add NA values
      tsibble::fill_gaps() %>%
      # Remove the NA's put at the end, so that the forecast starts from the last day with an observation,
      # rather than today
      dplyr::filter(time < forecast_starts$start_date)  %>%
      fabletools::model(RW = fable::RW(observed))
    
    # Generate the forecast
    RW_forecast <- RW_model %>%
      fabletools::generate(h = as.numeric(forecast_starts$h),
                           bootstrap = T,
                           times = 200) %>%
      rename(model_id = .model,
             predicted = .sim,
             ensemble = .rep) %>%
      as_tibble() %>% 
      mutate(#h = as.numeric(time - min(time) + 1),
        start_time = start) 
    
    message('RW forecast for ', start, ' at ', depth_use, ' m')
    return(RW_forecast)
  }  else {
    message('RW forecast not run for ', start, ' at ', depth_use, ' m')
  }
}

##################################################################
# now run the forecasts
# read in the targets data
depths <- c(1, 10)

###### first run temperature
targets <- read_csv('https://s3.flare-forecast.org/targets/sunp/sunp-targets-insitu.csv') %>%
  filter(variable == 'temperature',
         depth %in% depths) 

## for 2021
forecast_dates <- seq(ymd('2021-08-03'), ymd('2021-10-17'), 1)

forecast_vars <- expand.grid(start = forecast_dates, depth_use = unique(targets$depth)) %>%
  mutate(#targets = 'targets',
    h = 35)
forecast_vars
### first for temperature
RW_21_t <- purrr::pmap_dfr(forecast_vars, forecast.RW) %>%
  mutate(site_id = 'sunp',
         variable = 'temperature',
         forecast = 0,
         variable_type = 'state')


### now for 2022 temperature
forecast_dates <- seq(ymd('2022-08-03'), ymd('2022-10-17'), 1)
forecast_vars <- expand.grid(start = forecast_dates, depth_use = unique(targets$depth)) %>%
  mutate(#targets = 'targets',
    h = 35)

RW_22_t <- purrr::pmap_dfr(forecast_vars, forecast.RW) %>%
  mutate(site_id = 'sunp',
         variable = 'temperature',
         forecast = 0,
         variable_type = 'state')

# combine all temp forecasts
RW_temp <- rbind(RW_21_t, RW_22_t)

################################################################
### now for oxygen
targets <- read_csv('https://s3.flare-forecast.org/targets/sunp/sunp-targets-insitu.csv') %>%
  filter(variable == 'oxygen',
         depth %in% depths) 

## for 2021
forecast_dates <- seq(ymd('2021-08-03'), ymd('2021-10-17'), 1)

forecast_vars <- expand.grid(start = forecast_dates, depth_use = unique(targets$depth)) %>%
  mutate(#targets = 'targets',
    h = 35)

RW_21_o <- purrr::pmap_dfr(forecast_vars, forecast.RW) %>%
  mutate(site_id = 'sunp',
         variable = 'oxygen',
         forecast = 0,
         variable_type = 'state')

## for 2022 oxygen
forecast_dates <- seq(ymd('2022-08-03'), ymd('2022-10-17'), 1)

forecast_vars <- expand.grid(start = forecast_dates, depth_use = unique(targets$depth)) %>%
  mutate(#targets = 'targets',
    h = 35)

RW_22_o <- purrr::pmap_dfr(forecast_vars, forecast.RW) %>%
  mutate(site_id = 'sunp',
         variable = 'oxygen',
         forecast = 0,
         variable_type = 'state')

RW_oxy <- rbind(RW_21_o, RW_22_o)

RW_all <- rbind(RW_temp, RW_oxy)

# format for FLARE scoring function
RW_all <- RW_all %>% 
  mutate(pub_time = Sys.time(),
         site_id = 'sunp',
         family = 'ensemble') %>% 
  rename(reference_datetime = start_time,
         datetime = time,
         parameter = ensemble,
         prediction = predicted) %>% 
  select(reference_datetime, pub_time, model_id, site_id, depth, datetime,
         family, parameter, variable, prediction, forecast, variable_type) %>% 
  mutate(reference_datetime = as.character(as.POSIXct(reference_datetime) + 4*60*60))

attr(RW_all$reference_datetime, "tzone") <- "UTC"

write_csv(RW_all, './forecasts/sunp/RW.csv.gz')

obs <- read.csv('./targets/sunp/sunp-targets-insitu.csv')
obs <- obs %>% 
  mutate(time = as.POSIXct(time)) %>% 
  rename(reference_datetime = time)

########## score forecasts
score_file_RW <- FLAREr::generate_forecast_score(
  targets_file = './targets/sunp/sunp-targets-insitu.csv',
  forecast_file = './forecasts/sunp/RW.csv.gz',
  output_directory = './scores')

scored_RW <- read_parquet(score_file_RW)
scored_RW$reference_datetime <- as.POSIXct(scored_RW$reference_datetime)

RW_obs <- left_join(scored_RW, obs, by = c("reference_datetime", "depth", "variable"))
RW_obs_scored <- RW_obs %>% 
  select(model_id, reference_datetime, datetime, variable, depth, mean, sd, horizon, observed) %>% 
  ungroup()

RW_obs_scored <- na.omit(RW_obs_scored)

RW_obs_scored <- RW_obs_scored %>% 
  group_by(depth, variable, reference_datetime) %>% 
  mutate(crps = crps.numeric(y = observed, family = "normal", 
                           mean = mean, sd = sd))

RW_obs_scored %>% 
  mutate(doy = yday(reference_datetime),
         year = year(reference_datetime)) %>% 
  ggplot(aes(x = doy, y = crps, color = horizon)) +
  geom_line() +
  facet_wrap(depth~variable, scales = 'free')

scores_summ <- RW_obs_scored %>% 
  group_by(horizon, variable, depth) %>% 
  mutate(mean_crps = mean(crps),
         mean_crps = ifelse(variable=='temperature', mean_crps, mean_crps*32/1000))

write.csv(RW_obs_scored, './scores/sunp/RW_scores.csv')


