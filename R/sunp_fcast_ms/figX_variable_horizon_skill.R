#install.packages('Metrics')

library(lubridate)
library(tidyverse)
library(ggpubr)
library(arrow)
library(scales)
library(Metrics)

lake_directory <- here::here()

# some subsetting variables
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)
horizons <- c(1:35)
sim_name <- 'SUNP_fsed_deep_DA' 
folders <- c('all_UC_fsed_deep_DA')

########################################################################
# read in the scores and calculate variance
score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp", sim_name, folders[1]))

sc <- arrow::open_dataset(score_dir) |> 
  filter(variable %in% vars,
         depth %in% depths) %>% 
  collect() 

for(i in 1:length(folders)){
  score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp", sim_name, folders[1]))
  
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
         as.Date(reference_datetime) %in% buoy_dates) %>% 
  select(model_id, reference_datetime, datetime, horizon, depth, variable, everything()) 

# convert oxy crps and obs to mg/L
sc <- sc %>% 
  mutate(crps = ifelse(variable=='temperature', crps, (crps*32/1000)),
         mean = ifelse(variable=='temperature', mean, (mean*32/1000)),
         observation = ifelse(variable=='temperature', observation, (observation*32/1000)))




###############################################################################################
## read in climatology
# read in climatology and calculate mean scores
clim <- read.csv(file.path(lake_directory, 'scores/sunp/climatology_scores.csv'))
clim <- clim %>% 
  select(time, depth, variable, crps) %>% 
  rename(crps_clim = crps,
         datetime = time) %>% 
  mutate(datetime = as.Date(datetime),
         depth = as.numeric(depth))

sc <- sc %>% 
  select(datetime:variable, crps) %>% 
  mutate(datetime = as.Date(datetime))

sc_clim <- full_join(sc, clim, by = c('datetime', 'depth', 'variable'))
sc_clim <- sc_clim %>% 
  mutate(nCRPS = 1 - (crps/crps_clim))


#### aggregate across horizon
mean_hzon_var <- sc_clim %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, horizon) %>% 
  mutate(mean_crps = mean(nCRPS, na.rm = TRUE),
         sd_crps = sd(nCRPS, na.rm = TRUE)) %>% 
  distinct(variable, horizon, depth, .keep_all = TRUE) %>% 
  select(variable, horizon, depth, mean_crps:sd_crps)

ggplot(mean_hzon_var, aes(x = horizon, y = mean_crps, linetype = variable)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 0)) +
  #geom_ribbon(aes(ymax = mean_crps + sd_crps, ymin = mean_crps - sd_crps, fill = variable), alpha = 0.2) +
  ylab("Forecast Skill") +
  theme_bw() 

####### calculate the mean across all horizons
mean_var <- sc_clim %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable) %>% 
  mutate(mean_crps = mean(nCRPS, na.rm = TRUE),
         sd_crps = sd(nCRPS, na.rm = TRUE)) %>% 
  distinct(variable, .keep_all = TRUE) %>% 
  select(variable, mean_crps:sd_crps)

skill_diff <- plyr::ddply(mean_hzon_var, c("variable"), function(x){
  diff = max(x$mean_crps, na.rm = TRUE) - min(x$mean_crps, na.rm = TRUE)
  return(diff = diff)
})

skill_diff
pct_increase <- skill_diff$V1[skill_diff$variable=='oxygen']/skill_diff$V1[skill_diff$variable=='temperature']
pct_increase

(skill_diff$V1[skill_diff$variable=='oxygen'] - skill_diff$V1[skill_diff$variable=='temperature'])/skill_diff$V1[skill_diff$variable=='oxygen']

#### look across depth
#### aggregate across horizon
mean_hzon_var_depth <- sc_clim %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, horizon, depth) %>% 
  mutate(mean_crps = mean(nCRPS, na.rm = TRUE),
         sd_crps = sd(nCRPS, na.rm = TRUE)) %>% 
  distinct(variable, horizon, depth, .keep_all = TRUE) %>% 
  select(variable, horizon, depth, mean_crps:sd_crps)

ggplot(mean_hzon_var_depth, aes(x = horizon, y = mean_crps, linetype = variable)) +
  geom_line() +
  facet_wrap(~depth, ncol = 1) +
  geom_hline(aes(yintercept = 0)) +
  ylab("Forecast Skill") +
  theme_bw() 


#### look across depth
#### and across horizon
#### and year
mean_hzon_var_depth_yr <- sc_clim %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  mutate(year = year(datetime)) %>% 
  group_by(variable, horizon, depth, year) %>% 
  mutate(mean_crps = mean(nCRPS, na.rm = TRUE),
         sd_crps = sd(nCRPS, na.rm = TRUE)) %>% 
  distinct(variable, horizon, depth, .keep_all = TRUE) %>% 
  select(variable, horizon, depth, mean_crps:sd_crps)


ggplot(mean_hzon_var_depth_yr, aes(x = as.factor(fct_rev(variable)), y = mean_crps, color = as.factor(year))) +
  geom_boxplot() +
  facet_wrap(~depth, ncol = 1) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  ylab("Forecast Skill") +
  theme_bw()  +
  xlab('Variable') +
  labs(color = 'Year')

skill_fig <- ggplot(mean_hzon_var_depth_yr, aes(x = horizon, y = mean_crps, linetype = variable, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  #facet_wrap(~depth, ncol = 1) +
  geom_ribbon(aes(ymax = mean_crps + sd_crps, ymin = mean_crps - sd_crps, fill = as.factor(year)), alpha = 0.5) +
  facet_grid(depth~fct_rev(variable)) +
  geom_hline(aes(yintercept = 0)) +
  ylab("Forecast Skill") +
  theme_bw() +
  theme(panel.spacing = unit(0.5, "cm")) +
  labs(color = 'Year', linetype = 'Variable') +
  guides(fill = FALSE)

# look at mean differences across depth for each year
mean_overall <- sc_clim %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  mutate(year = year(datetime)) %>% 
  group_by(variable, depth, year) %>% 
  mutate(mean_crps = mean(nCRPS, na.rm = TRUE),
         sd_crps = sd(nCRPS, na.rm = TRUE)) %>% 
  distinct(variable, depth, .keep_all = TRUE) %>% 
  select(variable, depth, mean_crps:sd_crps)

ggplot(mean_overall, aes(x = as.factor(depth), y = mean_crps)) +
  geom_point() +
  facet_grid(year~variable)

skill_wide <- mean_overall %>% 
  select(-sd_crps) %>% 
  pivot_wider(names_from = year, values_from = mean_crps)

ggplot(skill_wide) +
  geom_segment(aes(x=fct_rev(as.factor(depth)), xend=fct_rev(as.factor(depth)), y=`2021`, yend=`2022`), color="grey") +
  geom_point(aes(y=`2021`, x=fct_rev(as.factor(depth)), color = '2021'), size=3 ) +
  geom_point(aes(y=`2022`, x=fct_rev(as.factor(depth)), color = '2022'), size=3 ) +
  coord_flip()+
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~fct_rev(variable), ncol = 1) +
  theme_bw() +
  xlab('Depth') +
  ylab('Skill') +
  labs(color = 'Year')

skill_wide$metric <- "Skill"

#write.csv(skill_wide, './data_processed/skill.csv', row.names = FALSE)
#######################################################################################################################
## for each horizon, calculate the percent of forecasts better than null (above 0)
sc_clim$year <- year(sc_clim$datetime)
sc_clim <- na.omit(sc_clim)
pct_null <- plyr::ddply(sc_clim, c("variable", "depth", "year", "horizon"), function(x){
  better <- x %>% 
    filter(x$nCRPS >= 0)
  worse <- x %>% 
    filter(x$nCRPS < 0)
  pct <- nrow(better)/(nrow(better) + nrow(worse))
  return(pct)
})

pct_null$pct <- round(pct_null$V1, 2)*100

pct_fig <- ggplot(pct_null, aes(x = horizon, y = pct, linetype = variable, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  #facet_wrap(~depth, ncol = 1) +
  facet_grid(depth~fct_rev(variable)) +
  ylab("% of Forecasts \n Better than Null") +
  theme_bw() +
  theme(panel.spacing = unit(0.5, "cm")) +
  labs(linetype = 'Variable', color = 'Year')
pct_fig

pct_null %>% 
  group_by(variable, year, depth) %>% 
  mutate(mean_pct = mean(pct)) %>% 
ggplot(aes(x = variable, y = mean_pct, shape = as.factor(depth), color = as.factor(year))) +
  geom_point(size = 3) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  #facet_wrap(~depth, ncol = 1) +
  ylab("% Forecasts Above Null") +
  theme_bw() +
  labs(linetype = 'Variable', color = 'Year')


pct_by_depth <- pct_null %>% 
  group_by(variable, depth, year) %>% 
  mutate(mean_pct = mean(pct)) %>% 
  distinct(variable, depth, year, .keep_all = TRUE)
pct_by_depth


ggarrange(skill_fig, pct_fig, common.legend = TRUE, nrow = 1)

#############################################################
## add the PE panel

library(statcomp)
library(ggpmisc)

lake_directory <- here::here()

d <- read.csv(file.path(lake_directory, "targets/sunp/SUNP_fsed_deep_DA/sunp-targets-insitu.csv"))

d <- d %>% 
  mutate(doy = yday(time),
         year = year(time)) %>% 
  filter(doy > 246,
         depth %in% c(1, 10),
         year %in% c(2021, 2022))

## go with demb = 4
PE <- plyr::ddply(d, c("depth", "year", "variable"), \(x) {
  print(head(x))
  data = x$observed
  opd = ordinal_pattern_distribution(x = data, ndemb = 4)
  pe <- permutation_entropy(opd)
  data.frame(ndemb = 4, pe = pe)
})
PE

PE_wide <- PE %>% 
  pivot_wider(names_from = year, values_from = pe)

PE_fig <- ggplot(PE_wide) +
  geom_segment(aes(x=fct_rev(as.factor(depth)), xend=fct_rev(as.factor(depth)), y=`2021`, yend=`2022`), color="grey") +
  geom_point(aes(y=`2021`, x=fct_rev(as.factor(depth)), color = '2021'), size=3 ) +
  geom_point(aes(y=`2022`, x=fct_rev(as.factor(depth)), color = '2022'), size=3 ) +
  coord_flip()+
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~fct_rev(variable), ncol = 1) +
  theme_bw() +
  xlab('Depth') +
  ylab('Permutation Entropy') +
  labs(color = 'Year')

ggarrange(skill_fig, pct_fig, PE_fig, labels = 'auto', common.legend = TRUE, nrow = 1)

