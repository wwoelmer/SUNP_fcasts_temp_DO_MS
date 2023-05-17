#install.packages('ggpubr')
#install.packages('lubridate')
#install.packages('rMR')

library(lubridate)
library(tidyverse)
library(ggpubr)
library(arrow)
library(scales)
library(rMR)
library(scoringRules)
library(Metrics)

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

#########################################################################
# check that time duration is the same for both years
ggplot(sc[sc$depth%in%c(1, 10),], 
       aes(x = doy, y = observation, color = as.factor(year))) +
  geom_point() +
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) 

####################################################################################
# fig 2
# temp
t <- ggplot(sc[sc$depth%in%c(1, 10) & sc$variable=='temperature (C)'
              # & sc$horizon==35
               ,], aes(x = as.factor(year), y = crps, fill = as.factor(year))) +
  geom_violin() +
  facet_wrap(~depth, scale = 'free', ncol = 1) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  ggtitle('Temperature (°C)') + 
  labs(fill = 'Year') +
  xlab('Year') +
  #ylim(0, 3.6) +
  #ylim(0.25, 1.8) +
 # ylim(0.4, 2.4) +
  ylab ('CRPS (°C)') +
  #scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  stat_summary(fun = "median",
               geom = "point",
               color = "black")  +
 # stat_compare_means() +
  theme_bw()
t

o <- ggplot(sc[sc$depth%in%c(1, 10) & sc$variable=='oxygen (mg/L)'
              # & sc$horizon==35
               ,], aes(x = as.factor(year), y = crps, fill = as.factor(year))) +
  geom_violin() +
  facet_wrap(~depth, scale = 'free', ncol = 1) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  ggtitle('Oxygen (mg/L)') + 
  labs(fill = 'Year') +
  xlab('Year') +
  ylab ('CRPS (mg/L)') +
  #ylim(0, 2.3) +
  #ylim(0.3, 1) +
  #ylim(0.4, 1.8) +
  stat_summary(fun = "median",
               geom = "point",
               color = "black")  +
   #stat_compare_means() +
  #scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme_bw()
o

ggarrange(t, o, common.legend = TRUE)

##############################
# boxplots of the median forecast value to compare with obs
tf1 <- sc %>% 
  filter(depth %in% c(1,10),
         variable=='temperature (C)',
         horizon==1) %>% 
  ggplot(aes(x = as.factor(year), y = median, fill = as.factor(year))) +
  geom_violin() +
  facet_wrap(~depth, scale = 'free', ncol = 1) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  ggtitle('Temperature (°C)') + 
  labs(fill = 'Year') +
  xlab('Year') +
  ylim(12, 27) +
  ylab ('CRPS (°C)') +
  #scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  stat_summary(fun = "median",
               geom = "point",
               color = "black")  +
  # stat_compare_means() +
  theme_bw()

of1 <- sc %>% 
  filter(depth %in% c(1,10),
         variable=='oxygen (mg/L)',
         horizon==1) %>% 
  ggplot(aes(x = as.factor(year), y = median*32/1000, fill = as.factor(year))) +
  geom_violin() +
  facet_wrap(~depth, scale = 'free', ncol = 1) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  ggtitle('Temperature (°C)') + 
  labs(fill = 'Year') +
  xlab('Year') +
  ylim(7, 10) +
  ylab ('CRPS (mg/L)') +
  #scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  stat_summary(fun = "median",
               geom = "point",
               color = "black")  +
  # stat_compare_means() +
  theme_bw()

ggarrange(tf1, of1, common.legend = TRUE)

###############################
# mean, min, max by year
summ_crps <- sc %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(year, depth, variable) %>% 
  dplyr::summarise(mean = mean(crps, na.rm = TRUE), 
                   median = median(crps, na.rm = TRUE),
                   min = min(crps, na.rm = TRUE),
                   max = max(crps, na.rm = TRUE),
                   range = abs(min - max))

summ_depth <- sc %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(depth, variable) %>% 
  dplyr::summarise(mean = mean(crps, na.rm = TRUE), 
                   min = min(crps, na.rm = TRUE),
                   max = max(crps, na.rm = TRUE),
                   range = abs(min - max))

####################################################################
## now a figure with log score instead of crps
ggplot(sc[sc$depth%in%c(1, 10),], aes(x = as.factor(year), y = logs, fill = as.factor(year))) +
  geom_boxplot() +
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  ggtitle('all horizons') + 
  labs(fill = 'Year') +
  xlab('Year') +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  stat_compare_means() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))


#######################################################################
## calculate % saturation instead of mg/L
ox <- sc %>% 
  filter(variable=='oxygen (mg/L)') %>% 
  select(model_id:observation, mean, sd) %>% 
  mutate(row = row_number())%>%   
  pivot_wider(names_from = variable, values_from = c(observation, mean, sd))

temp <- sc %>% 
  filter(variable=='temperature (C)') %>% 
  select(model_id:observation) %>% 
  mutate(row = row_number())%>%   
  pivot_wider(names_from = variable, values_from = c(observation))

sat <- left_join(ox, temp) 
sat <- sat %>% 
  dplyr::rename('obs_mgL' = "observation_oxygen (mg/L)",
                'mean_mgL' = "mean_oxygen (mg/L)",
                'sd_mgL' = "sd_oxygen (mg/L)",
                'temp_C' = "temperature (C)") 
sat <- sat %>% 
  mutate(obs_sat = DO.saturation(obs_mgL, temp_C, elevation.m = 1093)*100,
         mean_sat = DO.saturation(mean_mgL, temp_C, elevation.m = 1093)*100,
         sd_sat = DO.saturation(sd_mgL, temp_C, elevation.m = 1093)*100,
         crps_sat = crps.numeric(obs_sat, family = "normal", mean = mean_sat, sd = sd_sat),
         log_sat = logs.numeric(obs_sat, family = "normal", mean = mean_sat, sd = sd_sat),
         rmse = Metrics::rmse(obs_sat, mean_sat),
         year = year(datetime),
         doy = yday(datetime),
         mo_day = format(as.Date(datetime), "%m-%d"))

a <- ggplot(data = sat, aes(x = as.Date(mo_day, format = "%m-%d"), y = obs_sat, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~depth, scales = 'free') +
  scale_x_date(date_labels = "%b") +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  ylab('DO (% Sat)') +
  xlab('Date') +
  labs(color = 'Year')
a
b <- ggplot(data = sat, aes(x = as.factor(year), y = obs_sat)) +
  facet_wrap(~depth) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  geom_boxplot(aes(group = year, fill = as.factor(year))) +
  ylab('DO (% Sat)') +
  xlab('Year') +
  labs(fill = 'Year')

ggarrange(a, b, common.legend = TRUE)


ggplot(sat, aes(x = as.factor(year), y = log_sat, fill = as.factor(year))) +
  geom_boxplot() +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  ggtitle('all horizons') + 
  labs(fill = 'Year') +
  xlab('Year') +
  #stat_compare_means() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
sat_p <- ggplot(sat, aes(x = as.factor(year), y = crps_sat, fill = as.factor(year))) +
  geom_boxplot() +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~depth, ncol = 1, nrow = 2, scales = 'free') +
  ggtitle('Oxygen (% Sat)') + 
  labs(fill = 'Year') +
  ylab('CRPS (%)') +
  xlab('Year') +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  stat_compare_means() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
sat_p

ggarrange(o, sat_p)

####################################################################################
# fig 2, add jitter, violin
# temp
t <- ggplot(sc[sc$depth%in%c(1, 10) & sc$variable=='temperature (C)',], aes(x = as.factor(year), y = crps, fill = as.factor(year))) +
  geom_violin() +
  facet_wrap(~depth, ncol = 1) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  ggtitle('Temperature (°C)') + 
  labs(fill = 'Year') +
  xlab('Year') +
  #ylim(0, 4) +
  ylab ('CRPS (°C)') +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  stat_compare_means() +
  stat_summary(fun = "median",
               geom = "crossbar",
               color = "black", 
               width = 0.3)  +  
  theme_bw()
t

o <- ggplot(sc[sc$depth%in%c(1, 10) & sc$variable=='oxygen (mg/L)',], aes(x = as.factor(year), y = crps, fill = as.factor(year))) +
  geom_violin() +
  facet_wrap(~depth, ncol = 1) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  ggtitle('Oxygen (mg/L)') + 
  labs(fill = 'Year') +
  xlab('Year') +
  ylab ('CRPS (mg/L)') +
  stat_summary(fun = "median",
               geom = "crossbar",
               color = "black")  +  
  stat_compare_means() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme_bw()
o

ggarrange(t, o, common.legend = TRUE)
