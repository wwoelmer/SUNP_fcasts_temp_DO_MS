library(tidyverse)
#install.packages('ggpubr')
library(ggpubr)
library(arrow)

lake_directory <- here::here()

# some subsetting variables
vars <- c('temperature', 'oxygen')
depths <- c(0.1, 1.0, 10.0, 30.0)
horizons <- c(1:35)
folders <- c('all_UC', 'initial_condition', 'observation', 'parameter', 'process', 'weather')
#folders <- c('initial_condition')

########################################################################
# read in the scores and calculate variance
score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp/UC_analysis_2021", folders[1]))

sc <- arrow::open_dataset(score_dir) |> 
  filter(variable %in% vars,
         depth %in% depths) %>% 
  collect() 

for(i in 1:length(folders)){
  score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp/UC_analysis_2021", folders[i]))
  
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

# make vector of dates when obs are available (before buoy is taken into harbor)
buoy_dates <- c(seq.Date(as.Date('2021-08-04'), as.Date('2021-10-19'), by = 'day'),
                seq.Date(as.Date('2022-06-01'), as.Date('2022-10-17'), by = 'day'))

sc <- sc %>% 
  mutate(doy = yday(datetime),
         year = year(datetime)) %>% 
  select(-c(family, site_id)) %>% 
  filter(horizon > 0,
         as.Date(datetime) %in% buoy_dates) %>% 
  select(model_id, reference_datetime, datetime, horizon, depth, variable, everything()) 

# convert oxy crps to mg/L
sc <- sc %>% 
  mutate(crps = ifelse(variable=='temperature', crps, (crps*32/1000)),
         observation_mgL_C = ifelse(variable=='temperature', observation, (observation*32/1000))) 


##########################################################################
# reliability diagrams
rd <- sc %>% 
  select(reference_datetime, datetime, depth, model_id, variable, horizon,
         observation, mean, sd, doy, year) %>% 
  filter(!is.na(observation),
         variable=="oxygen", # all grouping variables
         horizon==35,   
         depth==10)


dat <- data.frame(bin = seq(10, 90, by = 10),
                  obs_freq = NA,
                  low = c(0.45, 0.40, 0.35, 0.3, 0.25, 0.20, 0.15, 0.10, 0.05),
                  high = c(0.55, 0.60, 0.65, 0.7, 0.75, 0.80, 0.85, 0.9, 0.95),
                  UC = 'all_UC')


for(i in 2:length(folders)){
  temp <- data.frame(bin = seq(10, 90, by = 10),
                    obs_freq = NA,
                    low = c(0.45, 0.40, 0.35, 0.3, 0.25, 0.20, 0.15, 0.10, 0.05),
                    high = c(0.55, 0.60, 0.65, 0.7, 0.75, 0.80, 0.85, 0.9, 0.95),
                    UC = folders[i])
  dat <- rbind(dat, temp)
}


vec <- c()

for(k in 1:length(folders)){
  temp <- rd[rd$model_id==folders[k],]
  temp_dat <- dat[dat$UC==folders[k],]
  for(j in 1:length(unique(dat$bin))){
    for(i in 1:nrow(temp)){
      # subset to forecasts within each break range of the distribution
      prob_above <- qnorm(dat$high[j], temp$mean[i], temp$sd[i])
      prob_below <- qnorm(dat$low[j], temp$mean[i], temp$sd[i])
      
      
      if(temp$observation[i] < prob_above & temp$observation[i] > prob_below){
        # 1 if obs in in CI, 0 if not
        vec <- c(vec, 1)
      }else{
        vec <- c(vec, 0)
      }
      
      
    }
    temp_dat$obs_freq[j] <- sum(vec)/nrow(temp)*100
    vec <- c()
    ids <- which(dat$UC==temp_dat$UC)
    dat[ids,] <- temp_dat
  }
  
}


ggplot(dat, aes(x = bin, y = bin, color = UC)) +
  geom_point(color = 'black') +
  geom_line(aes(y = obs_freq)) +
  ggtitle('oxygen 10m horizon = 35')

