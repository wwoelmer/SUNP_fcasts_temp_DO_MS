library(tidyverse)
library(ggpubr)
library(arrow)
library(lubridate)
library(scales)
library(Metrics)

lake_directory <- here::here()

# some subsetting variables
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0, 30.0)
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

#########################################################################
# compare forecast mean and observations
horizons <- c(1, 7, 14, 21, 35)
depths_obs <- c(1.0, 10.0)
sc2 <- sc %>% 
  filter(horizon%in%horizons,
         depth %in% depths_obs)

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

o_crps <- ggplot(sc2[sc2$model_id!='observation' & sc2$variable=='oxygen (mg/L)',], aes(x = as.factor(model_id), y = crps, fill = as.factor(model_id))) +
  geom_boxplot() +
  scale_fill_manual(values = c('#7D7C84', '#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  facet_wrap(~depth) +
  xlab('') +
  labs(fill = 'UC Type') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ggtitle('CRPS, oxygen (mg/L)')+
  theme(axis.text.x=element_text(vjust = 1))
o_crps

t_crps <- ggplot(sc2[sc2$model_id!='observation' & sc2$variable=='temperature (C)',], aes(x = as.factor(model_id), y = crps, fill = as.factor(model_id))) +
  geom_boxplot() +
  scale_fill_manual(values = c('#7D7C84', '#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  facet_wrap(~depth) +
  xlab("") +
  labs(fill = 'UC Type Included') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ggtitle('CRPS, temperature (C)')+
  theme(axis.text.x=element_text(vjust = 1))

ggarrange(t_crps, o_crps, common.legend = TRUE)

##########################################################
# and log
o_log <- ggplot(sc2[sc2$model_id!='observation' & sc2$variable=='oxygen (mg/L)',], aes(x = as.factor(model_id), y = logs, fill = as.factor(model_id))) +
  geom_boxplot() +
  scale_fill_manual(values = c('#7D7C84', '#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  facet_wrap(~depth) +
  xlab('') +
  labs(fill = 'UC Type') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ggtitle('Log, oxygen (mg/L)') +
  stat_compare_means(method = 'anova',
                     label = 'p.signif')+
  theme(axis.text.x=element_text(vjust = 1))
o_log

t_log <- ggplot(sc2[sc2$model_id!='observation' & sc2$variable=='temperature (C)',], aes(x = as.factor(model_id), y = logs, fill = as.factor(model_id))) +
  geom_boxplot() +
  scale_fill_manual(values = c('#7D7C84', '#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  facet_wrap(~depth) +
  xlab('') +
  labs(fill = 'UC Type') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ggtitle('Log, temperature (C)') +
  stat_compare_means(method = 'anova',
                     label = 'p.signif')+
  theme(axis.text.x=element_text(vjust = 1))
t_log

ggarrange(t_log, o_log, common.legend = TRUE)

#############################
# calculate RMSE
sc2 <- sc2 %>% 
  group_by(depth, variable, horizon, datetime, model_id) %>% 
  mutate(rmse = rmse(observation, mean))


##########################################################
# and rmse
o_rmse <- ggplot(sc2[sc2$model_id!='observation' & sc2$variable=='oxygen (mg/L)',], aes(x = as.factor(model_id), y = rmse*32/1000, fill = as.factor(model_id))) +
  geom_boxplot() +
  scale_fill_manual(values = c('#7D7C84', '#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  facet_wrap(~depth) +
  ylab('RSE (mg/L)') +
  xlab('') +
  labs(fill = 'UC Type') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ggtitle('RSE, oxygen (mg/L)') +
  stat_compare_means(method = 'anova',
                     label = 'p.signif')+
  theme(axis.text.x=element_text(vjust = 1))
o_rmse

t_rmse <- ggplot(sc2[sc2$model_id!='observation' & sc2$variable=='temperature (C)',], aes(x = as.factor(model_id), y = rmse*32/1000, fill = as.factor(model_id))) +
  geom_boxplot() +
  scale_fill_manual(values = c('#7D7C84', '#DBD56E', '#88AB75', '#2D93AD', '#DE8F6E'))+
  facet_wrap(~depth) +
  xlab('') +
  ylab('RSE (C)') +
  labs(fill = 'UC Type') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ggtitle('RSE, temperature (C)') +
  stat_compare_means(method = 'anova',
                     label = 'p.signif') +
  theme(axis.text.x=element_text(vjust = 1)) 
t_rmse

ggarrange(t_rmse, o_rmse, common.legend = TRUE)

ggarrange(t_log, t_crps, t_rmse,
          o_log, o_crps, o_rmse,
          align = "v",
          common.legend = TRUE)
