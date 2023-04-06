library(tidyverse)
#install.packages('ggpubr')
library(ggpubr)
library(arrow)
install.packages('lubridate')
library(lubridate)
library(scales)

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

sc$variable <- factor(sc$variable, levels = c('temperature', 'oxygen'), 
                      ordered = TRUE, labels = c('temperature (C)', 'oxygen (mg/L)'))

#########################################################################
# compare forecast mean and observations
horizons <- c(1, 7, 14, 21, 35)
depths_obs <- c(1.0, 10.0)
sc2 <- sc %>% 
  filter(horizon%in%horizons,
         depth %in% depths_obs)

ggplot(sc2[sc2$variable=='oxygen (mg/L)' & sc2$model_id!='all_UC',], aes(x = doy, y = crps, shape = as.factor(horizon))) +
  geom_point(aes(color = model_id)) +
  facet_grid(cols = vars(year), rows = vars(depth), scale = 'free') +
  ggtitle('oxygen') 

ggplot(sc2[sc2$variable=='temperature (C)' & sc2$model_id!='all_UC',], aes(x = doy, y = crps, shape = as.factor(horizon))) +
  geom_point(aes(color = model_id)) +
  facet_grid(cols = vars(year), rows = vars(depth), scale = 'free') +
  ggtitle('temperature') 

ggplot(sc[sc$variable=='oxygen (mg/L)' & sc$model_id=='all_UC',], aes(x = datetime, y = observation)) +
  geom_point() +
  facet_grid(cols = vars(year), rows = vars(depth), scale = 'free') +
  ggtitle('oxygen observations in forecasts')

ggplot(sc[sc$depth%in%c(1, 10),], aes(x = as.factor(year), y = crps, fill = as.factor(year))) +
  geom_boxplot() +
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  ggtitle('all horizons') + 
  labs(fill = 'Year') +
  xlab('Year') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))

ggplot(sc[sc$depth%in%c(1, 10),], aes(x = as.factor(year), y = crps, fill = as.factor(year))) +
  geom_violin() +
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  #stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  stat_summary(fun.data=mean_sdl, mult=1, 
               geom="pointrange") +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  ggtitle('all horizons') +
  labs(fill = 'Year') +
  xlab('Year') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))

ggplot(sc[sc$depth%in%c(1, 10),], aes(x = as.factor(year), y = logs, fill = as.factor(year))) +
  geom_boxplot() +
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  ggtitle('all horizons') +
  labs(fill = 'Year') +
  xlab('Year')
  
ggplot(sc[sc$depth%in%c(1, 10),], aes(x = as.factor(horizon), y = logs, fill = as.factor(year))) +
  geom_boxplot() +
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  ggtitle('all horizons') +
  labs(fill = 'Year') +
  xlab('Year')

mean_skill <- sc %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, horizon, depth, year) %>% 
  mutate(mean_crps = mean(crps, na.rm = TRUE),
         mean_log = mean(logs, na.rm = TRUE)) %>% 
  distinct(variable, horizon, depth, .keep_all = TRUE) %>% 
  select(variable, horizon, depth, mean_crps, mean_log)

out <- plyr::ddply(mean_skill, c("variable", "depth", "year"), function(x){
  
  slope <- coef(lm(x$mean_crps ~ x$horizon))[2]
  return(slope)
})
colnames(out) <- c('variable', 'depth', 'year', 'slope')
out$y <- c(1.5, 1.3, 2.0, 1.7, 1.5, 1.3, 2.0, 1.7)
out$x <- rep(9, 8)  

ggplot(mean_skill[mean_skill$variable=='oxygen (mg/L)',], aes(x = horizon, y = mean_log, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~depth) +
  xlab('horizon (days into future)') +
  ylab('log score') +
  labs(color = 'Year') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))

  
ggplot(mean_skill, aes(x = horizon, y = mean_crps, color = as.factor(year))) +
  geom_line() +
  stat_smooth(method = "lm") +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  geom_text(data = out, aes(label = paste0('slope = ', round(slope, 3)), x= x, y = y)) +
  xlab('horizon (days into future)') +
  ylab('CRPS') +
  labs(color = 'Year') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
  
### add climatology 
scores <- read.csv('./scores/sunp/climatology_scores.csv')

means <- scores %>% 
  filter(as.Date(time) %in% buoy_dates) %>% 
  group_by(variable, depth, year) %>% 
  mutate(mean = mean(score)) %>% 
  distinct(variable, year, depth, .keep_all = TRUE) %>% 
  dplyr::select(variable, depth, year, mean)
means$variable <- factor(means$variable, levels = c('temperature', 'oxygen'), 
                         ordered = TRUE, labels = c('temperature (C)', 'oxygen (mg/L)'))

ggplot(mean_skill, aes(x = horizon, y = mean_crps, color = as.factor(year))) +
  geom_line() +
  stat_smooth(method = "lm") +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  geom_text(data = out, aes(label = paste0('slope = ', round(slope, 3)), x= x, y = y)) +
  geom_hline(data = means, aes(yintercept = mean, color = as.factor(year))) +
  xlab('horizon (days into future)') +
  ylab('CRPS') +
  labs(color = 'Year') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))

####
oxy1 <- mean_skill[mean_skill$variable=='oxygen (mg/L)' & mean_skill$depth==1,]
lm(oxy1$mean_crps ~ oxy1$horizon)

# decrease in skill from 1-35 days oxygen at 1m
((0.1705369 - 0.4889371)/0.1705369)*100 #1day/35day

# decrease in skill from 1-35 days oxygen at 10m 
((0.2276311 - 1.3242497)/0.2276311)*100

# decrease in skill from 1-35 days temperautre at 1m 
(0.2169309 - 1.3643646)/0.2169309*100

# decrease in skill from 1-35 days oxygen at 10m 
(0.4086960 - 2.0609075)/0.4086960*100

mean_by_year <- sc %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(year, variable) %>% 
  mutate(mean_crps = mean(crps, na.rm = TRUE)) %>% 
  distinct(year, variable, .keep_all = TRUE) %>% 
  select(year, variable, mean_crps)
mean_by_year

ggplot(sc2, aes(x = as.factor(year), y = crps, fill = as.factor(horizon))) +
  geom_boxplot() +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') +
  ggtitle('select horizons') +
  xlab('horizon (days into future)') +
  ylab('CRPS') +
  labs(fill = 'horizon')

ggplot(sc2, aes(x = as.factor(horizon), y = crps, fill = as.factor(year))) +
  geom_boxplot() +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') +
  ggtitle('select horizons')

###############
# look at crps as a function of uncertainty type
ggplot(sc2, aes(x = as.factor(year), y = crps, fill = as.factor(model_id))) +
  geom_boxplot() +
  scale_fill_manual(values = c('#7D7C84', '#DBD56E', '#414288', '#2D93AD', '#88AB75', '#DE8F6E'))+
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  xlab('Year') +
  labs(fill = 'UC Type') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))


ggplot(sc2[sc2$model_id!='observation',], aes(x = as.factor(year), y = crps, fill = as.factor(model_id))) +
  geom_boxplot() +
  scale_fill_manual(values = c('#7D7C84', '#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  xlab('Year') +
  labs(fill = 'UC Type') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
  

o_uc <- ggplot(sc2[sc2$model_id!='observation' & sc2$variable=='oxygen (mg/L)',], aes(x = as.factor(year), y = crps, fill = as.factor(model_id))) +
  geom_boxplot() +
  scale_fill_manual(values = c('#7D7C84', '#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  facet_wrap(~depth) +
  xlab('Year') +
  labs(fill = 'UC Type') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black")) +
  ggtitle('oxygen (mg/L)')

t_uc <- ggplot(sc2[sc2$model_id!='observation' & sc2$variable=='temperature (C)',], aes(x = as.factor(year), y = crps, fill = as.factor(model_id))) +
  geom_boxplot() +
  scale_fill_manual(values = c('#7D7C84', '#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  facet_wrap(~depth) +
  xlab('Year') +
  labs(fill = 'UC Type') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black")) +
  ggtitle('temperature (C)')

ggarrange(t_uc, o_uc, common.legend = TRUE)

ggplot(sc2[sc2$model_id=='all_UC' & sc2$horizon==7,], aes(x = doy, y = crps, color = as.factor(year))) +
  geom_line() +
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') +
  ggtitle('horizon = 7')


############################
# look at observations and mean + CI behavior within each UC mode
ggplot(sc2[sc2$variable=='oxygen (mg/L)' & sc2$year==2021 & sc2$horizon==7,], aes(x = doy, y = mean)) +
  geom_line(aes(color = model_id)) +
  geom_ribbon(aes(ymin = quantile02.5, ymax = quantile97.5, fill = model_id, color = model_id)) +
  geom_point(aes(y = observation)) +
  facet_grid(cols = vars(model_id), rows = vars(depth), scale = 'free') +
  ggtitle('oxygen, 2021')

ggplot(sc2[sc2$variable=='oxygen (mg/L)' & sc2$year==2022 & sc2$horizon==7,], aes(x = doy, y = mean)) +
  geom_line(aes(color = model_id)) +
  geom_ribbon(aes(ymin = quantile02.5, ymax = quantile97.5, fill = model_id, color = model_id)) +
  geom_point(aes(y = observation)) +
  facet_grid(cols = vars(model_id), rows = vars(depth), scale = 'free') +
  ggtitle('oxygen, 2022')

ggplot(sc2[sc2$variable=='temperature (C)' & sc2$year==2021 & sc2$horizon==7,], aes(x = doy, y = mean)) +
  geom_line(aes(color = model_id)) +
  geom_ribbon(aes(ymin = quantile02.5, ymax = quantile97.5, fill = model_id, color = model_id)) +
  geom_point(aes(y = observation)) +
  facet_grid(cols = vars(model_id), rows = vars(depth), scale = 'free') +
  ggtitle('temperature, 2021')

ggplot(sc2[sc2$variable=='temperature (C)' & sc2$year==2022 & sc2$horizon==7,], aes(x = doy, y = mean)) +
  geom_line(aes(color = model_id)) +
  geom_ribbon(aes(ymin = quantile02.5, ymax = quantile97.5, fill = model_id, color = model_id)) +
  geom_point(aes(y = observation)) +
  facet_grid(cols = vars(model_id), rows = vars(depth), scale = 'free') +
  ggtitle('temperature, 2022')

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

ggplot(uncert_sum[uncert_sum$variable=='oxygen (mg/L)' & uncert_sum$model_id!='observation',], aes(x = horizon, y = variance, fill = model_id)) +
  geom_bar(position = 'stack', stat = 'identity') +
  facet_grid(cols = vars(year), rows = vars(depth), scale = 'free') +
  ggtitle('oxygen') +
  scale_fill_manual(values = c('#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))


ggplot(uncert_sum[uncert_sum$variable=='temperature (C)' & uncert_sum$model_id!='observation',], aes(x = horizon, y = variance, fill = model_id)) +
  geom_bar(position = 'stack', stat = 'identity') +
  facet_grid(cols = vars(year), rows = vars(depth), scale = 'free') +
  ggtitle('temperature') +
  scale_fill_manual(values = c('#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))

# plot variance over day of year
ggplot(uncert_sum[uncert_sum$year==2021 & uncert_sum$horizon==7,], aes(x = doy, y = variance, fill = model_id)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') +
  xlim(150, 320) +
  ggtitle('2021')
  
ggplot(uncert_sum[uncert_sum$year==2022 & uncert_sum$horizon==7,], aes(x = doy, y = variance, fill = model_id)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') +
  xlim(150, 320) +
  ggtitle('2022')

## also look at patterns separated by variable
ggplot(uncert_sum[uncert_sum$variable=='oxygen (mg/L)',], aes(x = doy, y = variance, fill = model_id)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(cols = vars(year), rows = vars(depth), scale = 'free') +
  xlim(150, 320) +
  ggtitle('oxygen')

ggplot(uncert_sum[uncert_sum$variable=='temperature (C)',], aes(x = doy, y = variance, fill = model_id)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(cols = vars(year), rows = vars(depth), scale = 'free') +
  xlim(150, 320) +
  ggtitle('temperature')


ggplot(uncert_sum[uncert_sum$variable=='temperature (C)',], aes(x = as.Date(datetime), y = crps)) +
  geom_point(aes(color = model_id)) +
  facet_grid(cols = vars(year), rows = vars(depth), scale = 'free') +
  ggtitle('temperature')

ggplot(uncert_sum[uncert_sum$variable=='oxygen (mg/L)',], aes(x = doy, y = crps)) +
  geom_point(aes(color = model_id)) +
  facet_grid(cols = vars(year), rows = vars(depth), scale = 'free') +
  ggtitle('oxygen')

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

both <- ggplot(uncert_mean, aes(x = horizon, y = mean_prop, fill = model_id)) +
  geom_bar(stat = 'identity', position= 'stack', width = 1) +
  facet_grid(cols = vars(variable), rows = vars(depth)) +
  ggtitle('Both years')+
  ylab('Proportion of Variance') +
  labs(fill = 'UC Type') +
  scale_fill_manual(values = c('#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))
both


# look at proportional dynamics WITHOUT process UC
uncert_sum_noprocess <- sc %>% 
  filter(model_id!='all_UC',
         model_id!='process',
         !is.na(observation)) %>% 
  group_by(model_id, variable, depth, datetime, horizon) %>% 
  mutate(variance = sd^2) %>% 
  distinct(variable, depth, datetime, horizon, .keep_all = TRUE) 

uncert_prop_noprocess <- uncert_sum_noprocess %>% 
  group_by(variable, depth, datetime, horizon) %>% 
  mutate(total_var = sum(variance)) %>% 
  mutate(var_prop = variance/total_var)

# aggregate over each horizon 
uncert_mean_noprocess <- uncert_prop_noprocess %>% 
  group_by(horizon, variable, depth, model_id, year) %>% 
  mutate(mean_prop = mean(var_prop)) %>% 
  distinct(horizon, variable, depth, model_id, .keep_all = TRUE)  

y21 <- ggplot(uncert_mean_noprocess[uncert_mean_noprocess$year==2021,], aes(x = horizon, y = mean_prop, fill = model_id)) +
  geom_bar(stat = 'identity', position= 'stack') +
  facet_grid(cols = vars(variable), rows = vars(depth)) +
  ggtitle('2021')

y22 <- ggplot(uncert_mean_noprocess[uncert_mean_noprocess$year==2022,], aes(x = horizon, y = mean_prop, fill = model_id)) +
  geom_bar(stat = 'identity', position= 'stack') +
  facet_grid(cols = vars(variable), rows = vars(depth)) +
  ggtitle('2022')

y21
y22

# subset down to a few horizons
horizon_sub <- c(1, 7, 20, 35)
uncert_prop2 <- uncert_prop %>% 
  filter(horizon %in% horizon_sub)

ggplot(uncert_prop2[uncert_prop2$depth==1,], 
       aes(x = as.POSIXct(datetime), y = var_prop, fill = model_id)) +
  geom_area() +
  facet_grid(cols = vars(variable), rows = vars(horizon)) +
  ggtitle("1.0m")

ggplot(uncert_prop2[uncert_prop2$depth==10,], 
       aes(x = as.POSIXct(datetime), y = var_prop, fill = model_id)) +
  geom_area() +
  facet_grid(cols = vars(variable), rows = vars(horizon)) +
  ggtitle("10.0m")

ggplot(uncert_prop2[uncert_prop2$variable=='oxygen (mg/L)' & uncert_prop2$year==2021,], 
       aes(x = doy, y = var_prop, fill = model_id)) +
  geom_area() +
  facet_grid(cols = vars(horizon), rows = vars(depth)) +
  ggtitle("oxygen in 2021 (horizon in columns, depth in rows)")

ggplot(uncert_prop2[uncert_prop2$variable=='oxygen (mg/L)' & uncert_prop2$year==2022,], 
       aes(x = doy, y = var_prop, fill = model_id)) +
  geom_area() +
  facet_grid(cols = vars(horizon), rows = vars(depth)) +
  ggtitle("oxygen in 2022 (horizon in columns, depth in rows)")

ggplot(uncert_prop2[uncert_prop2$variable=='temperature (C)' & uncert_prop2$year==2022,], 
       aes(x = doy, y = var_prop, fill = model_id)) +
  geom_area() +
  facet_grid(cols = vars(horizon), rows = vars(depth)) +
  ggtitle("temperature in 2022 (horizon in columns, depth in rows)")

ggplot(uncert_prop2[uncert_prop2$variable=='oxygen (mg/L)' & uncert_prop2$horizon==1,], 
       aes(x = doy, y = var_prop, fill = model_id)) +
  geom_area() +
  facet_grid(cols = vars(year), rows = vars(depth)) +
  ggtitle("oxygen at horizon = 1")

ggplot(uncert_prop2[uncert_prop2$variable=='oxygen (mg/L)' & uncert_prop2$horizon==7,], 
       aes(x = doy, y = var_prop, fill = model_id)) +
  geom_area() +
  facet_grid(cols = vars(year), rows = vars(depth)) +
  ggtitle("oxygen at horizon = 1")

#########################################################################
colsc <- seq_gradient_pal('lightblue', 'darkblue', 'Lab')(seq(0, 1, length.out = 4))

uncert_same_days <- uncert_prop2 %>% 
  filter(doy > 217 & doy < 291)

oxy <- ggplot(uncert_same_days[uncert_same_days$horizon==1 & uncert_same_days$variable=='oxygen (mg/L)',], aes(x = as.factor(year), y = var_prop, color = as.factor(depth), fill = as.factor(depth))) +
  geom_boxplot() +
  scale_color_manual(values = colsc) +
  scale_fill_manual(values = colsc) +
  facet_wrap(~model_id, scales = 'free') +
  ggtitle('oxygen (mg/L)') +
  xlab('Year') +
  ylab('Proportion of Variance') +
  labs(fill = 'Depth', color = 'Depth')
oxy

temp <- ggplot(uncert_same_days[uncert_same_days$horizon==1 & uncert_same_days$variable=='temperature (C)',], aes(x = as.factor(year), y = var_prop, color = as.factor(depth), fill = as.factor(depth))) +
  geom_boxplot() +
  scale_color_manual(values = colsc) +
  scale_fill_manual(values = colsc) +
  facet_wrap(~model_id, scales = 'free') +
  ggtitle('temperature (C)') +
  xlab('Year') +
  ylab('Proportion of Variance') +
  labs(fill = 'Depth', color = 'Depth')
temp

p <- ggarrange(temp, oxy, common.legend = TRUE) 
annotate_figure(p, top = text_grob("Variability across DOY, Horizon = 1",
                                   face = "bold", 
                                   size = 14))

##########################################################################
ggplot(uncert_prop2[uncert_prop2$horizon==1 & uncert_prop2$variable=='oxygen (mg/L)' & uncert_prop2$model_id!='observation',], aes(x = model_id, y = var_prop, fill = as.factor(year))) +
  geom_boxplot() +
  facet_wrap(~depth, scales = 'free')


ggplot(uncert_prop2[uncert_prop2$horizon==1 & uncert_prop2$variable=='oxygen (mg/L)' & uncert_prop2$model_id!='observation',], aes(x = as.factor(year), y = var_prop, fill = as.factor(model_id))) +
  geom_boxplot() +
  scale_fill_manual(values = c('#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  facet_wrap(~depth, scales = 'free')

ggplot(uncert_prop2[uncert_prop2$horizon==1 & uncert_prop2$variable=='temperature (C)' & uncert_prop2$model_id!='observation',], aes(x = as.factor(year), y = var_prop, fill = as.factor(model_id))) +
  geom_boxplot() +
  scale_fill_manual(values = c('#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  facet_wrap(~depth, scales = 'free')

ggplot(uncert_prop2[uncert_prop2$horizon==1 & uncert_prop2$model_id!='observation',], aes(x = as.factor(year), y = var_prop, fill = as.factor(model_id))) +
  geom_boxplot() +
  scale_fill_manual(values = c('#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  facet_grid(cols = vars(variable), rows = vars(depth)) 

ggplot(uncert_prop2[uncert_prop2$horizon==1 & uncert_prop2$variable=='oxygen (mg/L)',], aes(x = model_id, y = var_prop, fill = as.factor(year))) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  facet_wrap(~depth, scales = 'free')

ggplot(uncert_prop2[uncert_prop2$horizon==1 & uncert_prop2$variable=='temperature (C)',], aes(x = model_id, y = var_prop, fill = as.factor(year))) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  facet_wrap(~depth, scales = 'free')

ggplot(uncert_prop2[uncert_prop2$horizon==1,], aes(x = model_id, y = var_prop, fill = as.factor(year))) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  facet_grid(cols = vars(variable), rows = vars(depth)) 
  