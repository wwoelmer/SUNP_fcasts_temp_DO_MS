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
folder <- c('all_UC_fsed_deep_DA')

########################################################################
# read in the scores and calculate variance
score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp", folder))

sc <- arrow::open_dataset(score_dir) |> 
  filter(variable %in% vars,
         depth %in% depths) %>% 
  collect() 

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
## read in RW and calculate mean scores
rw_scores <- read.csv('./scores/sunp/RW_scored.csv')
rw_scores <- rw_scores %>% 
  mutate(datetime = as.Date(datetime)) %>% 
  mutate(crps_rw = ifelse(variable=='temperature', crps, crps*32/1000)) %>% 
  select(-crps)

sc <- sc %>% 
  select(datetime:variable, crps) %>% 
  mutate(datetime = as.Date(datetime))

sc_rw <- full_join(sc, rw_scores, by = c('datetime', 'depth', 'variable', 'horizon'))
sc_rw <- sc_rw %>% 
  mutate(nCRPS = 1 - (crps/crps_rw),
         year = year(datetime)) %>% 
  select(datetime, horizon, depth, variable, year, model_id,
         nCRPS)

##################################################################################################
### read in climatology 
clim <- read.csv(file.path(lake_directory, 'scores/sunp/climatology_scores.csv'))
clim <- clim %>% 
  select(time, depth, variable, crps) %>% 
  rename(crps_clim = crps,
         datetime = time) %>% 
  mutate(datetime = as.Date(datetime),
         depth = as.numeric(depth))

sc_clim <- full_join(sc, clim, by = c('datetime', 'depth', 'variable'))
sc_clim <- sc_clim %>% 
  mutate(nCRPS = 1 - (crps/crps_clim),
         model_id = 'clim',
         year = year(datetime)) %>% 
  select(-c(crps, crps_clim))


sc_all <- full_join(sc_rw, sc_clim, by = c('datetime', 'depth', 'variable', 'horizon', 
                                           'model_id', 'year', 'nCRPS'))

sc_all <- na.omit(sc_all)

mean_skill <- sc_all %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(variable, horizon, depth, year, model_id) %>% 
  mutate(mean_crps = mean(nCRPS, na.rm = TRUE),
         sd_crps = sd(nCRPS, na.rm = TRUE)) %>% 
  distinct(variable, horizon, depth, year, model_id, .keep_all = TRUE) %>% 
  select(variable, horizon, depth, mean_crps:sd_crps)


skill_fig <- ggplot(mean_skill, aes(x = horizon, y = mean_crps, linetype = model_id, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  #facet_wrap(~depth, ncol = 1) +
  #geom_ribbon(aes(ymax = mean_crps + sd_crps, ymin = mean_crps - sd_crps, fill = as.factor(year)), alpha = 0.5) +
  facet_grid(depth~fct_rev(variable)) +
  geom_hline(aes(yintercept = 0)) +
  ylab("Forecast Skill") +
  theme_bw() +
  theme(panel.spacing = unit(0.5, "cm")) +
  labs(color = 'Year', linetype = 'Variable') +
  guides(fill = FALSE)
skill_fig

# look at mean differences across depth for each year
mean_overall <- sc_all %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  mutate(year = year(datetime)) %>% 
  group_by(variable, depth, year, model_id) %>% 
  mutate(mean_crps = mean(nCRPS, na.rm = TRUE),
         sd_crps = sd(nCRPS, na.rm = TRUE)) %>% 
  distinct(variable, depth, model_id, .keep_all = TRUE) %>% 
  select(variable, depth, model_id, mean_crps:sd_crps)

ggplot(mean_overall, aes(x = as.factor(depth), y = mean_crps, shape = model_id)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  facet_grid(year~fct_rev(variable)) +
  ylab('Mean Skill') +
  theme_bw()


#######################################################################################################################
## for each horizon, calculate the percent of forecasts better than null (above 0)
sc_all$year <- year(sc_all$datetime)
sc_all <- na.omit(sc_all)
pct_null <- plyr::ddply(sc_all, c("variable", "depth", "year", "horizon", "model_id"), function(x){
  better <- x %>% 
    filter(x$nCRPS >= 0)
  worse <- x %>% 
    filter(x$nCRPS < 0)
  pct <- nrow(better)/(nrow(better) + nrow(worse))
  return(pct)
})

pct_null$pct <- round(pct_null$V1, 2)*100

pct_fig <- ggplot(pct_null, aes(x = horizon, y = pct, linetype = model_id, color = as.factor(year))) +
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
ggsave('./figures/pct_null.tiff', pct_fig, scale = 0.5, dpi = 300, unit = "mm", width = 250, height = 150)


pct_null %>% 
  group_by(variable, year, depth) %>% 
  mutate(mean_pct = mean(pct)) %>% 
  ggplot(aes(x = fct_rev(variable), y = mean_pct, shape = as.factor(depth), color = as.factor(year))) +
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

##########################################################################
## calculate the % of time FLARE beat either null
sc_all <- sc_all %>% 
  distinct(datetime, horizon, depth, variable, year, model_id, .keep_all = TRUE)
sc_wide <- sc_all %>% 
  pivot_wider(names_from = model_id, values_from = nCRPS)

sc_wide <- sc_wide %>% 
  mutate(classify = 
           ifelse(RW > 0 & clim < 0,
                  "RW",
                  ifelse(RW < 0 & clim > 0,
                         "clim",
                         ifelse(RW > 0 & clim > 0,
                                "both",
                                ifelse(RW < 0 & clim < 0,
                                       "neither",
                                       "ERROR")))))  
summ_null <- sc_wide %>% 
  group_by(classify, variable, depth, horizon) %>% 
  summarise(Percentage = n()/nrow(.)*100)

# some climatology forecasts are NA because observations are not available on those dates--remove these
summ_null <- na.omit(summ_null)

# set the order of the factor
summ_null$classify <- factor(summ_null$classify, levels = c('neither', 'clim', 'RW', 'both'))

ggplot(summ_null, aes(x = horizon, y = Percentage, color = classify, fill = classify)) +
  geom_bar(position = 'fill', stat = 'identity') +
  facet_grid(depth~fct_rev(variable)) +
  ylab("% of FLARE Forecasts \n Better than Null") +
  scale_color_brewer(palette = "Set3") +
  scale_fill_brewer(palette = 'Set3') +
  theme_bw() +
  theme(panel.spacing = unit(0.5, "cm")) +
  labs(fill = 'Comparison Model') +
  guides(color = FALSE)

pct_null_any <- plyr::ddply(sc_wide, c("variable", "depth", "year", "horizon"), function(x){
  better <- x %>% 
    filter(x$nCRPS >= 0)

  worse <- x %>% 
    filter(x$nCRPS < 0)
  pct <- nrow(better)/(nrow(better) + nrow(worse))
  return(pct)
})

pct_fig <- ggplot(pct_null_any, aes(x = horizon, y = V1, color = as.factor(year))) +
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

test <- sc_all %>% 
  filter(variable=='oxygen', depth==1, year==2021, horizon==1)
better <- test %>% 
  filter(test$nCRPS >= 0)
mods <- unique(better$model_id)
