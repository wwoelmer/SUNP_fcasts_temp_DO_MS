#install.packages('ggpubr')
#install.packages('lubridate')
library(tidyverse)
library(ggpubr)
library(arrow)
library(lubridate)
library(scales)

lake_directory <- here::here()

# some subsetting variables
vars <- c('temperature', 'oxygen')
depths <- c(0.1, 1.0, 10.0, 30.0)
horizons <- c(1:35)
folders <- c('all_UC', 'initial_condition', 'observation', 'parameter', 'process', 'weather')

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

# make vector of dates when obs are available (before buoy is taken into harbor)
buoy_dates <- c(seq.Date(as.Date('2021-08-04'), as.Date('2021-10-17'), by = 'day'),
                seq.Date(as.Date('2022-08-04'), as.Date('2022-10-17'), by = 'day'))

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

sc$variable <- factor(sc$variable, levels = c('temperature', 'oxygen'), 
                      ordered = TRUE, labels = c('temperature (C)', 'oxygen (mg/L)'))

#############################################################################
# compare uncertainty dynamics

uncert_sum <- sc %>% 
  filter(model_id!='all_UC',
         depth!=0.1) %>% 
  group_by(model_id, variable, depth, datetime, horizon) %>% 
  mutate(variance = sd^2) %>% 
  distinct(variable, depth, datetime, horizon, .keep_all = TRUE) 

y21 <- ggplot(uncert_sum[uncert_sum$year==2021 & uncert_sum$model_id!='observation',], aes(x = horizon, y = variance, fill = model_id)) +
  geom_bar(position = 'stack', stat = 'identity', width = 1) +
  scale_fill_manual(values = c('#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') +
  ggtitle('2021') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
y21

y22 <- ggplot(uncert_sum[uncert_sum$year==2022 & uncert_sum$model_id!='observation',], aes(x = horizon, y = variance, fill = model_id)) +
  geom_bar(position = 'stack', stat = 'identity', width = 1) +
  scale_fill_manual(values = c('#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') +
  ggtitle('2022') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
y22

ggarrange(y21, y22, common.legend = TRUE)

# plot variance over day of year
o_21 <- uncert_sum %>% 
  filter(year==2021,
         horizon==1,
         variable=='oxygen (mg/L)',
         model_id!='observation') %>% 
ggplot(aes(x = doy, y = variance, fill = model_id)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  scale_fill_manual(values = c('#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  ggtitle('2021')

o_var <- uncert_sum %>% 
  filter(horizon==1,
         variable=='oxygen (mg/L)',
         model_id!='observation') %>% 
  ggplot(aes(x = doy, y = variance*32/1000, fill = model_id)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +
  facet_grid(cols = vars(year), rows = vars(depth), scale = 'free') +
  scale_fill_manual(values = c('#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  ggtitle('Oxygen (mg/L)')

t_var <- uncert_sum %>% 
  filter(horizon==1,
         variable=='temperature (C)',
         model_id!='observation') %>% 
  ggplot(aes(x = doy, y = variance, fill = model_id)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +
  facet_grid(cols = vars(year), rows = vars(depth), scale = 'free') +
  scale_fill_manual(values = c('#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  ggtitle('Temperature (C)')
# day 231 is the weird high day in 2022

ggarrange(t_var, o_var,
          common.legend = TRUE)

## also look at patterns separated by variable
ggplot(sc[sc$model_id=='all_UC' & sc$horizon==7,], aes(x = doy, y = mean, color = as.factor(year))) +
  geom_line() +
  geom_point(aes(x = doy, y = observation)) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') 


uncert_prop <- uncert_sum %>% 
  filter(model_id!='observation') %>% 
  group_by(variable, depth, datetime, horizon) %>% 
  mutate(total_var = sum(variance)) %>% 
  mutate(var_prop = variance/total_var)

# aggregate over each horizon 
uncert_mean <- uncert_prop %>% 
  group_by(horizon, variable, depth, model_id) %>% 
  mutate(mean_prop = mean(var_prop)) %>% 
  distinct(horizon, variable, depth, model_id, .keep_all = TRUE)  

# and calculate total var for each year
uncert_prop_year <- uncert_sum %>% 
  filter(model_id!='observation') %>% 
  group_by(variable, depth, datetime, horizon, year) %>% 
  mutate(total_var = sum(variance)) %>% 
  mutate(var_prop = variance/total_var)

uncert_mean_year <- uncert_prop_year %>% 
  group_by(horizon, variable, depth, year) %>% 
  mutate(mean_prop = mean(var_prop)) %>% 
  distinct(horizon, variable, depth, year, .keep_all = TRUE)


both <- ggplot(uncert_mean, aes(x = horizon, y = mean_prop, fill = model_id)) +
  geom_bar(stat = 'identity', position= 'stack', width = 1) +
  #geom_line(data = uncert_mean_year, aes(x = horizon, y = total_var, color = as.factor(year))) +
  facet_grid(cols = vars(variable), rows = vars(depth)) +
  ggtitle('Both years')+
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  ylab('Proportion of Variance') +
  labs(fill = 'UC Type') +
  scale_fill_manual(values = c('#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
both

ggplot(uncert_mean_year[uncert_mean_year$variable=='temperature (C)',], aes(x = horizon, y = total_var, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~depth) 


ggplot(uncert_mean_year[uncert_mean_year$variable=='oxygen (mg/L)',], aes(x = horizon, y = total_var, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~depth) 


