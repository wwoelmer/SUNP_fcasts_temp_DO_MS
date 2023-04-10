#install.packages('ggpubr')
#install.packages('lubridate')

library(lubridate)
library(tidyverse)
library(ggpubr)
library(arrow)
library(scales)

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
  score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp", folders[i]))
  
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
  filter(horizon > 0,
         as.Date(datetime) %in% buoy_dates) %>% 
  select(model_id, reference_datetime, datetime, horizon, depth, variable, everything()) 

# convert oxy crps and obs to mg/L
sc <- sc %>% 
  mutate(crps = ifelse(variable=='temperature', crps, (crps*32/1000)),
         observation = ifelse(variable=='temperature', observation, (observation*32/1000)))

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
t <- ggplot(sc[sc$depth%in%c(1, 10) & sc$variable=='temperature (C)',], aes(x = as.factor(year), y = crps, fill = as.factor(year))) +
  geom_boxplot() +
  facet_wrap(~depth, scale = 'free', ncol = 1) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  ggtitle('Temperature (°C)') + 
  labs(fill = 'Year') +
  xlab('Year') +
  ylab ('CRPS (°C)') +
  #stat_compare_means() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
t

o <- ggplot(sc[sc$depth%in%c(1, 10) & sc$variable=='oxygen (mg/L)',], aes(x = as.factor(year), y = crps, fill = as.factor(year))) +
  geom_boxplot() +
  facet_wrap(~depth, scale = 'free', ncol = 1) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  ggtitle('Oxygen (mg/L)') + 
  labs(fill = 'Year') +
  xlab('Year') +
  ylab ('CRPS (mg/L)') +
  #stat_compare_means() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
o

ggarrange(t, o, common.legend = TRUE)

####################################################################
## now a figure with log score instead of crps
ggplot(sc[sc$depth%in%c(1, 10),], aes(x = as.factor(year), y = logs, fill = as.factor(year))) +
  geom_boxplot() +
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  ggtitle('all horizons') + 
  labs(fill = 'Year') +
  xlab('Year') +
  #stat_compare_means() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
