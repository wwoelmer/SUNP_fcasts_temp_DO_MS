# script to create targets file (observations) 
# and generate climatology forecast from historical observations

library(scoringRules)
library(tidyverse)
library(ggpubr)
library(FLAREr)

lake_directory <- here::here()
setwd(lake_directory)

tgts <- read.csv('./targets/sunp/SUNP_fcasts_temp_DO/sunp-targets-insitu.csv')

# set 1.5m obs of oxygen to 1.0m, convert oxy to mg/L
tgts <- tgts %>% 
  mutate(depth = ifelse(variable=='oxygen' & depth==1.5, 1.0, depth),
         observed = ifelse(variable=='oxygen', observed*32/1000, observed), # convert from mmol/m3 to mg/L
         depth = ifelse(variable=='temperature' & depth==1.5, 1.0, depth))

depths <- c(1.0, 10.0)
tgts <- tgts %>% 
  filter(depth %in% depths) %>% 
  mutate(doy = yday(time),
         year = year(time)) 

null <- tgts %>% 
  filter(year < 2021) %>% # only use observations before forecast period
  group_by(depth, variable, doy) %>% 
  mutate(mean = mean(observed, na.rm = TRUE),
         sd = sd(observed, na.rm = TRUE),
         n = n()) %>% 
  filter(n > 2) %>% # requireds more than two observations to calculate a mean/sd
  distinct(depth, variable, doy, .keep_all = TRUE) %>% 
  dplyr::select(doy, variable, depth, mean, sd, n)


# calculate scores for 2021
t_2021 <- tgts %>% 
  filter(year==2021,
         time > as.Date('2021-06-07')) #doy > 218 & doy < 292
t_2021 <- full_join(t_2021, null)
t_2021 <- t_2021 %>% 
  filter(observed > 0,
         sd > 0) %>% 
  group_by(depth, variable, doy) %>% 
  mutate(crps = crps.numeric(y = observed, family = "normal", 
                              mean = mean, sd = sd),
         rmse = Metrics::rmse(observed, mean))

#######################
# calculate scores for 2022
t_2022 <- tgts %>% 
  filter(year==2022) # doy > 154 & doy < 290
t_2022 <- full_join(t_2022, null)
t_2022 <- t_2022 %>% 
  filter(observed > 0,
         sd > 0) %>% 
  group_by(depth, variable, doy) %>% 
  mutate(crps = crps.numeric(y = observed, family = "normal", 
                             mean = mean, sd = sd),
         rmse = Metrics::rmse(observed, mean))

# combine scores
scores <- full_join(t_2021, t_2022)

write.csv(scores, './scores/sunp/climatology_scores.csv', row.names = FALSE)


