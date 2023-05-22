## plot skill metrics over doy

library(lubridate)
library(tidyverse)
library(ggpubr)
library(arrow)
library(scales)
library(rMR)
library(scoringRules)
library(Metrics)
#install.packages('RcppRoll')
library(RcppRoll)

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
         as.Date(reference_datetime) %in% buoy_dates,
         doy < 291) %>%
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

# add month day
sc <- sc %>% 
  mutate(mo_day = format(datetime, "%m-%d"))


#####
mtc <- sc %>% 
  select(reference_datetime:mean, sd, year, rmse)


### select horizons for all metrics
ggplotly(sc %>% 
  filter(horizon %in% c(1, 7, 14, 21, 35)) %>% 
ggplot(aes(x = doy, y = crps, color = as.factor(horizon), linetype = as.factor(year))) +
  geom_line() +
  facet_grid(depth~variable))

sc %>% 
  filter(horizon %in% c(1, 7, 14, 21, 35)) %>% 
  ggplot(aes(x = doy, y = logs, color = as.factor(horizon), linetype = as.factor(year))) +
  geom_line() +
  facet_grid(depth~variable)

#####################
# plot crps with pattern of observations from each year
sc %>% 
  filter(horizon %in% c(1)) %>% 
  ggplot(aes(x = doy, y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'horizon', linetype = 'year') +
  facet_wrap(depth~variable, scales = 'free') +
  geom_line(aes(y = observation/5, color = as.factor(year), linetype = 'obs'))+
  scale_y_continuous(name = 'CRPS', sec.axis = sec_axis(trans = ~.*5, name = 'observations'))
  

brks <- seq.Date(as.Date('08-04', "%m-%d"), as.Date('10-17', '%m-%d'), by = "1 month")

################################################3
## LSPA POSTER

t <- sc %>% 
  filter(horizon %in% c(21),
         variable=='temperature (C)',
         doy > 238) %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1, scales = 'free') +
  ylab('Forecast Performance (°C)') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  theme_bw()
o <- sc %>% 
  filter(horizon %in% c(21),
         variable=='oxygen (mg/L)',
         doy > 238) %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(~depth, ncol = 1, scales = 'free') +
  ylab('Forecast Performance (mg/L)') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  theme_bw()
ggarrange(t, o, common.legend = TRUE)

#########################################################

sc %>% 
  filter(horizon %in% c(1)) %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(depth~variable, scales = 'free',
             labeller = label_wrap_gen(multi_line = FALSE)) +
  ylab('Forecast Performance (CRPS)') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  ggtitle('horizon = 1') +
  theme_bw()

sc %>% 
  filter(horizon %in% c(10)) %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(depth~variable, scales = 'free',
             labeller = label_wrap_gen(multi_line = FALSE)) +
  ylab('Forecast Performance (CRPS)') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  ggtitle('horizon = 10')+
  theme_bw()

sc %>% 
  filter(horizon %in% c(21)) %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(depth~variable, scales = 'free',
             labeller = label_wrap_gen(multi_line = FALSE)) +
  ylab('Forecast Performance (CRPS)') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  ggtitle('horizon = 21')+
  theme_bw()

sc %>% 
  filter(horizon %in% c(35)) %>% 
  ggplot(aes(x = as.Date(mo_day, format = "%m-%d"), y = crps, color = as.factor(year), linetype = 'crps')) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  facet_wrap(depth~variable, scales = 'free',
             labeller = label_wrap_gen(multi_line = FALSE)) +
  ylab('Forecast Performance (CRPS)') +
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  xlab('Day of Year') +
  guides(linetype = "none") +
  ggtitle('horizon = 35')+
  theme_bw()
  
##########################################################################
## calculate difference betwee years
###
diff_yr <- sc %>% 
  ungroup() %>% 
  select(horizon:variable, observation, year, doy, crps, logs, rmse) %>% 
  distinct(horizon, depth, variable, year, doy, .keep_all = TRUE) %>% 
  mutate(year = factor(year))
diff_yr_wide <- diff_yr %>% 
  pivot_wider(names_from = year, values_from = c(crps, logs, rmse, observation))

diff_yr_wide %>% 
  filter(horizon %in% c(1, 7, 14, 21, 35)) %>% 
  ggplot(aes(x = crps_2021, y = crps_2022, color = as.factor(horizon))) +
  geom_line() +
  geom_abline(slope = 1) +
  geom_smooth(method = 'lm') +
  facet_wrap(depth~variable, scales = 'free',
             labeller = label_wrap_gen(multi_line = FALSE)) 

diff_yr_wide %>% 
  filter(horizon %in% c(1, 7, 14, 21, 35)) %>% 
  ggplot(aes(x = doy, y = crps_2021 - crps_2022, color = as.factor(horizon))) +
  geom_line() +
  facet_wrap(depth~variable, scales = 'free',
             labeller = label_wrap_gen(multi_line = FALSE)) +
  geom_hline(yintercept = 0)

diff_yr_wide %>% 
  filter(horizon %in% c(1, 7, 14, 21, 35)) %>% 
  ggplot(aes(x = doy, y = logs_2021 - logs_2022, color = as.factor(horizon))) +
  geom_line() +
  facet_wrap(depth~variable, scales = 'free',
             labeller = label_wrap_gen(multi_line = FALSE)) +
  geom_hline(yintercept = 0) 

diff_yr_wide %>% 
  filter(horizon %in% c(1)) %>% 
  ggplot(aes(x = doy, y = logs_2021 - logs_2022, color = 'log')) +
  geom_line() +
  facet_grid(depth~variable, scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = doy, y = observation_2021 - observation_2022, color = 'obs')) +
  scale_color_manual(values = c('orange', 'black'))

diff_yr_wide %>% 
  filter(horizon %in% c(7)) %>% 
  ggplot(aes(x = doy, y = logs_2021 - logs_2022, color = 'log')) +
  geom_line() +
  facet_grid(depth~variable, scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = doy, y = observation_2021 - observation_2022, color = 'obs')) +
  scale_color_manual(values = c('orange', 'black'))

diff_yr_wide %>% 
  filter(horizon %in% c(35)) %>% 
  ggplot(aes(x = doy, y = logs_2021 - logs_2022, color = 'log')) +
  geom_line() +
  facet_grid(depth~variable, scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = doy, y = observation_2021 - observation_2022, color = 'obs')) +
  scale_color_manual(values = c('orange', 'black'))

#####
# CRPS
diff_yr_wide %>% 
  filter(horizon %in% c(1)) %>% 
  ggplot(aes(x = doy, y = logs_2021 - logs_2022, color = 'log')) +
  geom_line() +
  facet_grid(depth~variable, scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = doy, y = observation_2021 - observation_2022, color = 'obs')) +
  scale_color_manual(values = c('orange', 'black'))

diff_yr_wide %>% 
  filter(horizon %in% c(7)) %>% 
  ggplot(aes(x = doy, y = logs_2021 - logs_2022, color = 'log')) +
  geom_line() +
  facet_grid(depth~variable, scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = doy, y = observation_2021 - observation_2022, color = 'obs')) +
  scale_color_manual(values = c('orange', 'black'))

diff_yr_wide %>% 
  filter(horizon %in% c(35)) %>% 
  ggplot(aes(x = doy, y = crps_2021 - crps_2022, color = 'crps')) +
  geom_line() +
  facet_grid(depth~variable, scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = doy, y = observation_2021 - observation_2022, color = 'obs')) +
  scale_color_manual(values = c('orange', 'black'))

##################################################
### running CV of observations
library(zoo)
#install.packages('ggpmisc')
library(ggpmisc)
cv <- na.omit(sc)

ggplotly(ggplot(cv, aes(x = doy, y = observation, color = as.factor(horizon), group = as.factor(year))) +
  geom_line() +
  facet_wrap(depth~variable, scales = 'free'))

#####
# test that the rollmean is doing what I think it is
tst <- cv %>% 
  filter(depth==1,
         variable=="oxygen (mg/L)",
         year==2021,
         horizon %in% c(1, 7)) %>% 
  select(reference_datetime:mean, doy:mo_day) %>% 
  group_by(horizon, depth, variable, year) %>% 
  mutate(roll_mean_obs = zoo::rollmean(observation, k = 10, fill = NA),
         roll_sd_obs = rollapply(observation, width = 10, FUN = sd, fill = NA),
         roll_mean_crps = rollmean(crps, k = 10, fill = NA),
         roll_sd_crps = rollapply(crps, width = 10, FUN = sd, fill = NA)) %>% 
  filter(!is.na(roll_mean_obs),
         doy > min(doy) + 7,
         doy < max(doy) - 7) %>% 
  mutate(roll_cv_crps = roll_sd_crps/roll_mean_crps,
         roll_cv_obs = roll_sd_obs/roll_mean_obs) %>% 
  distinct(horizon, depth, variable, year, doy, .keep_all = TRUE)


roll_cv <- cv %>% 
  select(reference_datetime:mean, doy:mo_day) %>% 
  group_by(horizon, depth, variable, year) %>% 
  mutate(roll_mean_obs = zoo::rollmean(observation, k = 10, fill = NA),
         roll_sd_obs = rollapply(observation, width = 10, FUN = sd, fill = NA),
         roll_mean_crps = rollmean(crps, k = 10, fill = NA),
         roll_sd_crps = rollapply(crps, width = 10, FUN = sd, fill = NA)) %>% 
  filter(!is.na(roll_mean_obs),
         doy > min(doy) + 10,
         doy < max(doy) - 10) %>% 
  mutate(roll_cv_crps = roll_sd_crps/roll_mean_crps,
         roll_cv_obs = roll_sd_obs/roll_mean_obs) %>% 
  distinct(horizon, depth, variable, year, doy, .keep_all = TRUE)


summ_cv <- cv %>% 
  group_by(year, variable, depth) %>% 
  mutate(cv_obs = sd(observation)/mean(observation),
         mean_cv = mean(cv_obs),
         cv_crps = sd(crps)/mean(crps),
         mean_cv_crps = mean(cv_crps)) %>% 
  distinct(year, variable, depth, .keep_all = TRUE) %>% 
  select(year, variable, depth, mean_cv, mean_cv_crps)

roll_cv %>% 
  filter(horizon == 1) %>% 
ggplot(aes(x = doy, y = roll_cv_obs, color = as.factor(year))) +
  geom_line() +
  facet_wrap(depth~variable, scales = 'free',
             labeller = label_wrap_gen(multi_line = FALSE)) +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year')

roll_cv %>% 
  filter(horizon == 1) %>% 
  ggplot(aes(x = doy, y = roll_cv_crps, color = as.factor(year))) +
  facet_wrap(depth~variable, scales = 'free',
             labeller = label_wrap_gen(multi_line = FALSE)) +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'Year') +
  geom_line()

#####################################################
### plot color with horizon, all years together

c_21 <- c( '#6EEEEB',  '#17BEBB', '#108E8B', '#0B6563')
c_22 <- c('#E69A96', '#D34C45', '#9E2B25', '#76201C')

t_21 <- roll_cv %>% 
  filter(variable=='temperature (C)',
         horizon %in% c(1, 10, 21, 35),
         year==2021) %>% 
  ggplot(aes(x = roll_cv_obs, y = roll_cv_crps,  color = as.factor(horizon))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('temperature') +
  facet_wrap(~depth, scales = 'free', ncol = 1) +
  scale_color_manual(values =  c_21) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(color = 'year') +
  stat_poly_eq(size = 2) +
  theme_bw()

o_21 <- roll_cv %>% 
  filter(variable=='oxygen (mg/L)',
         horizon %in% c(1, 10, 21, 35),
         year==2021) %>% 
  ggplot(aes(x = roll_cv_obs, y = roll_cv_crps, color = as.factor(horizon))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('oxygen') +
  facet_wrap(~depth, scales = 'free', ncol = 1) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  scale_color_manual(values =  c_21) +
  labs(color = 'year') +
  stat_poly_eq(size = 2) +
  theme_bw()

ggarrange(t_21, o_21, common.legend = TRUE)

###
t_22 <- roll_cv %>% 
  filter(variable=='temperature (C)',
         horizon %in% c(1, 10, 21, 35),
         year==2022) %>% 
  ggplot(aes(x = roll_cv_obs, y = roll_cv_crps, color = as.factor(horizon))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('temperature') +
  facet_wrap(~depth, scales = 'free', ncol = 1) +
  scale_color_manual(values =  c_22) +
  labs(color = 'year') +
  stat_poly_eq(size = 2) +
  theme_bw() 

o_22 <- roll_cv %>% 
  filter(variable=='oxygen (mg/L)',
         horizon %in% c(1, 10, 21, 35),
         year==2022) %>% 
  ggplot(aes(x = roll_cv_obs, y = roll_cv_crps, color = as.factor(horizon))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('oxygen') +
  facet_wrap(~depth, scales = 'free', ncol = 1) +
  scale_color_manual(values =  c_22) +
  labs(color = 'year') +
  stat_poly_eq(size = 2) +
  theme_bw()

ggarrange(t_22, o_22, common.legend = TRUE)



###### look at patterns without years separated

roll_cv %>% 
  filter(horizon %in% c(1, 10, 21, 35)) %>% 
  ggplot(aes(x = roll_cv_obs, y = roll_cv_crps)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(depth~variable, scales = 'free') +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(color = 'year') +
  stat_poly_eq()



#####
# look at one horizon across both years
t <- roll_cv %>% 
  filter(horizon==1,
         variable=='temperature (C)') %>% 
  ggplot(aes(x = roll_cv_obs, y = roll_cv_crps, color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('temperature') +
  facet_wrap(~depth, scales = 'free') +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'year') +
  stat_poly_eq() 
t

o <- roll_cv %>% 
  filter(horizon==1,
         variable=='oxygen (mg/L)') %>% 
  ggplot(aes(x = roll_cv_obs, y = roll_cv_crps, color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('oxygen') +
  facet_wrap(~depth, scales = 'free') +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'year') +
  stat_poly_eq() 
o
# Create a text grob
tgrob <- text_grob("Horizon = 1",size = 16)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,3,0,0, "cm"))

ggarrange(plot_0,NULL,t, o,
          ncol = 2,nrow = 2,heights = c(0.5,5),
          common.legend = TRUE)
ggarrange(t, o, common.legend = TRUE)

#######
## horizon = 10
t <- roll_cv %>% 
  filter(horizon==10,
         variable=='temperature (C)') %>% 
  ggplot(aes(x = roll_cv_obs, y = roll_cv_crps, color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('temperature') +
  facet_wrap(~depth, scales = 'free') +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'year') +
  stat_poly_eq() 
t

o <- roll_cv %>% 
  filter(horizon==10,
         variable=='oxygen (mg/L)') %>% 
  ggplot(aes(x = roll_cv_obs, y = roll_cv_crps, color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('oxygen') +
  facet_wrap(~depth, scales = 'free') +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'year') +
  stat_poly_eq() 
o


# Create a text grob
tgrob <- text_grob("Horizon = 10",size = 16)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,3,0,0, "cm"))

ggarrange(plot_0,NULL,t, o,
          ncol = 2,nrow = 2,heights = c(0.5,5),
          common.legend = TRUE)

#######
## horizon = 21
t <- roll_cv %>% 
  filter(horizon==21,
         variable=='temperature (C)') %>% 
  ggplot(aes(x = roll_cv_obs, y = roll_cv_crps, color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('temperature') +
  facet_wrap(~depth, scales = 'free') +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'year') +
  stat_poly_eq() 
t

o <- roll_cv %>% 
  filter(horizon==21,
         variable=='oxygen (mg/L)') %>% 
  ggplot(aes(x = roll_cv_obs, y = roll_cv_crps, color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('oxygen') +
  facet_wrap(~depth, scales = 'free') +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'year') +
  stat_poly_eq() 
o


# Create a text grob
tgrob <- text_grob("Horizon = 21",size = 16)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,3,0,0, "cm"))

ggarrange(plot_0,NULL,t, o,
          ncol = 2,nrow = 2,heights = c(0.5,5),
          common.legend = TRUE)

#######
## horizon = 35
t <- roll_cv %>% 
  filter(horizon==35,
         variable=='temperature (C)') %>% 
  ggplot(aes(x = roll_cv_obs, y = roll_cv_crps, color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('temperature') +
  facet_wrap(~depth, scales = 'free') +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'year') +
  stat_poly_eq() 
t

o <- roll_cv %>% 
  filter(horizon==35,
         variable=='oxygen (mg/L)') %>% 
  ggplot(aes(x = roll_cv_obs, y = roll_cv_crps, color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('oxygen') +
  facet_wrap(~depth, scales = 'free') +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  labs(color = 'year') +
  stat_poly_eq() 
o


# Create a text grob
tgrob <- text_grob("Horizon = 35",size = 16)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,3,0,0, "cm"))

ggarrange(plot_0,NULL,t, o,
          ncol = 2,nrow = 2,heights = c(0.5,5),
          common.legend = TRUE)
ggarrange(t, o, common.legend = TRUE)


roll_cv %>% 
  ggplot(aes(x = as.factor(year), y = roll_cv_obs, fill = as.factor(year))) +
  geom_violin() +
  facet_wrap(depth~variable, scales = 'free') +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) 
  
roll_cv %>% 
  ggplot(aes(x = as.factor(year), y = roll_cv_crps, fill = as.factor(year))) +
  geom_violin() +
  facet_wrap(depth~variable, scales = 'free') +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) 
