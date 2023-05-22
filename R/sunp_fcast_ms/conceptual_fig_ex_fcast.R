### extract example forecasts of each archetype (accuracy and precision axes)

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
  filter(horizon > 0,#) %>% 
         as.Date(reference_datetime) %in% buoy_dates) %>%
  select(model_id, reference_datetime, datetime, horizon, depth, variable, everything()) 

# convert oxy crps and obs to mg/L
sc <- sc %>% 
  mutate(crps = ifelse(variable=='temperature', crps, (crps*32/1000)),
         observation = ifelse(variable=='temperature', observation, (observation*32/1000)),
         mean = ifelse(variable=='temperature', mean, (mean*32/1000)),
         sd = ifelse(variable=='temperature', sd, (sd*32/1000)))

sc$variable <- factor(sc$variable, levels = c('temperature', 'oxygen'), 
                      ordered = TRUE, labels = c('temperature (C)', 'oxygen (mg/L)'))

### calculate rmse
sc <- sc %>% 
  group_by(depth, variable, horizon, datetime) %>% 
  mutate(rmse = rmse(observation, mean)) 


lp_la <- read.csv(paste0(lake_directory, '/forecasts/sunp/UC_analysis_2021/start_06_30/all_UC/sunp-2021-10-04-all_UC.csv.gz'))
lp_la <- lp_la %>% 
  mutate(horizon = difftime(as.POSIXct(datetime), as.POSIXct(reference_datetime), units = 'days')) %>% 
  filter(
    depth==10.0,
    variable=='oxygen')

obs <- sc %>% 
  filter(reference_datetime=='2021-10-04 00:00:00',
    depth==10.0,
    variable=='oxygen (mg/L)',
    horizon ==1)

score <- sc %>% 
  filter(reference_datetime=='2021-10-04 00:00:00',
         depth==10.0,
         variable=='oxygen (mg/L)')

a <- ggplot(score, aes(x = as.POSIXct(datetime), y = mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = quantile97.5*32/1000, ymax = quantile02.5*32/1000), alpha = 0.5) +
  geom_point(data = score, aes(x = as.POSIXct(datetime), y = observation, color = '')) +
  geom_point(data = obs, aes(x = datetime, y = observation, color = 'Observation')) +
  scale_color_manual(values = c('black', 'red')) +
  #scale_x_date(breaks = date_breaks('1 week')) +
  labs(color = "", alpha = "") +
  ylab('Forecast') +
  xlab('Date') +
  theme_bw()
a

b <- lp_la %>% 
  filter(horizon==1) %>% 
  ggplot(aes(x = prediction*32/1000, linetype = 'forecast')) +
  geom_density() +
  geom_point(data = obs, aes(x = observation, y = 0.01, color = 'Observation'), size = 3) +
  theme_bw() +
  scale_color_manual(values = c('red')) +
  xlab('Prediction (mg/L)') +
  ylab('Density')  +
  labs(color = "", linetype = "")
b
ggarrange(a, b, common.legend = TRUE, widths = c(0.75, 0.5))

###########################################################################################
lp_ha <- read.csv(paste0(lake_directory, '/forecasts/sunp/UC_analysis_2021/start_06_30/all_UC/sunp-2021-08-24-all_UC.csv.gz'))

lp_ha <- lp_ha %>% 
  mutate(horizon = difftime(as.POSIXct(datetime), as.POSIXct(reference_datetime), units = 'days')) %>% 
  filter(
    depth==10.0,
    variable=='temperature')

obs <- sc %>% 
  filter(reference_datetime=='2021-08-24 00:00:00',
         depth==10.0,
         variable=='temperature (C)',
         horizon ==24)

score <- sc %>% 
  filter(reference_datetime=='2021-08-24 00:00:00',
         depth==10.0,
         variable=='temperature (C)')

a <- ggplot(score, aes(x = as.POSIXct(datetime), y = mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = quantile97.5, ymax = quantile02.5), alpha = 0.5) +
  geom_point(data = score, aes(x = as.POSIXct(datetime), y = observation, color = '')) +
  geom_point(data = obs, aes(x = datetime, y = observation, color = 'Observation')) +
  scale_color_manual(values = c('black', 'red')) +
  #scale_x_date(breaks = date_breaks('1 week')) +
  labs(color = "", alpha = "") +
  ylab('Forecast') +
  xlab('Date') +
  theme_bw()
a

b <- lp_ha %>% 
  filter(horizon==24) %>% 
  ggplot(aes(x = prediction, linetype = 'forecast')) +
  geom_density() +
  geom_point(data = obs, aes(x = observation, y = 0, color = 'Observation'), size = 3) +
  theme_bw() +
  scale_color_manual(values = c('red')) +
  xlab('Prediction (C)') +
  ylab('Density')  +
  labs(color = "", linetype = "")
b
ggarrange(a, b, common.legend = TRUE)
###########################################################################################

hp_la
hp_ha