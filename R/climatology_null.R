install.packages('scoringRules')
library(scoringRules)
library(tidyverse)

setwd(here::here())

tgts <- read.csv('./targets/sunp/UC_analysis_2022/sunp-targets-insitu.csv')

ggplot(tgts, aes(x = depth)) +
  geom_histogram() +
  facet_wrap(~variable)

ggplot(tgts, aes(x = as.Date(time), y = observed)) +
  geom_line() +
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') 

# set 1.5m obs of oxygen to 1.0m, convert oxy to mg/L
tgts <- tgts %>% 
  mutate(depth = ifelse(variable=='oxygen' & depth==1.5, 1.0, depth),
         observed = ifelse(variable=='oxygen', observed*32/1000, observed))

depths <- c(1.0, 10.0)
tgts <- tgts %>% 
  filter(depth %in% depths) %>% 
  mutate(doy = yday(time),
         year = year(time)) 

null <- tgts %>% 
  group_by(depth, variable, doy) %>% 
  mutate(mean = mean(observed, na.rm = TRUE),
         sd = sd(observed, na.rm = TRUE),
         n = n()) %>% 
  distinct(depth, variable, doy, .keep_all = TRUE) %>% 
  dplyr::select(doy, variable, depth, mean, sd, n)

ggplot(null, aes(x = doy, y = mean, color = as.factor(depth))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean-sd, ymax = mean + sd, alpha = 0.3, fill = as.factor(depth))) +
  facet_wrap(~variable, scales = 'free')

t_2021 <- tgts %>% 
  filter(year==2021) #doy > 218 & doy < 292
t_2021 <- full_join(t_2021, null)
t_2021 <- t_2021 %>% 
  filter(observed > 0,
         sd > 0) %>% 
  group_by(depth, variable) %>% 
  mutate(score = crps.numeric(y = observed, family = "normal", 
                              mean = mean, sd = sd))

ggplot(t_2021[t_2021$variable=='oxygen',], aes(x = doy, y = score)) +
  geom_line() +
  facet_wrap(~depth, scales = 'free') +
  ggtitle('oxygen')

ggplot(t_2021[t_2021$variable=='temperature',], aes(x = doy, y = score)) +
  geom_line() +
  facet_wrap(~depth, scales = 'free') +
  ggtitle('temperature')

#######################
# 2022 only
t_2022 <- tgts %>% 
  filter(year==2022) # doy > 154 & doy < 290
t_2022 <- full_join(t_2022, null)
t_2022 <- t_2022 %>% 
  filter(observed > 0,
         sd > 0) %>% 
  group_by(depth, variable) %>% 
  mutate(score = crps.numeric(y = observed, family = "normal", 
                              mean = mean, sd = sd))

ggplot(t_2022[t_2022$variable=='oxygen',], aes(x = doy, y = score)) +
  geom_line() +
  facet_wrap(~depth, scales = 'free') +
  ggtitle('oxygen')

ggplot(t_2022[t_2022$variable=='temperature',], aes(x = doy, y = score)) +
  geom_line() +
  facet_wrap(~depth, scales = 'free') +
  ggtitle('temperature')

scores <- full_join(t_2021, t_2022)
write.csv(scores, './scores/sunp/climatology_scores.csv')


means <- scores %>% 
  group_by(variable, depth, year) %>% 
  mutate(mean = mean(score)) %>% 
  distinct(variable, year, depth, .keep_all = TRUE) %>% 
  dplyr::select(variable, depth, year, mean)
