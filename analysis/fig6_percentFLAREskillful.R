# code to recreate figure 6, percent of FLARE forecasts beating one, both, or neither null forecast

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
sim_name <- 'SUNP_fcasts_temp_DO' 

########################################################################
# read in the scores and calculate variance
score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp", sim_name))

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
rw_scores <- read.csv('./scores/sunp/RW_scores.csv')
rw_scores <- rw_scores %>% 
  mutate(datetime = as.Date(datetime)) %>% 
  mutate(crps_rw = ifelse(variable=='temperature', crps, crps*32/1000)) %>% 
  select(-crps)

mean_rw_skill <- rw_scores %>% 
  group_by(variable, depth) %>% 
  mutate(mean_crps = mean(crps_rw)) %>% 
  distinct(variable, depth, .keep_all = TRUE)

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

mean_clim_skill <- clim %>% 
  group_by(variable, depth) %>% 
  mutate(mean_crps = mean(crps_clim)) %>% 
  distinct(variable, depth, .keep_all = TRUE)

sc_clim <- full_join(sc, clim, by = c('datetime', 'depth', 'variable'))
sc_clim <- sc_clim %>% 
  mutate(nCRPS = 1 - (crps/crps_clim),
         model_id = 'clim',
         year = year(datetime)) %>% 
  select(-c(crps, crps_clim))


sc_all <- full_join(sc_rw, sc_clim, by = c('datetime', 'depth', 'variable', 'horizon', 
                                           'model_id', 'year', 'nCRPS'))

sc_all <- na.omit(sc_all)

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
sc_wide <- na.omit(sc_wide)

out <- plyr::ddply(sc_wide, c("variable", "depth", "horizon"), function(x){
  x %>% 
    group_by(classify) %>% 
    summarise(Percentage = n()/nrow(.)*100)
})

out$classify <- factor(out$classify, levels = c('neither', 'clim', 'RW', 'both'), 
                       labels = c('neither', 'climatology', 'persistence', 'both'))


# some climatology forecasts are NA because observations are not available on those dates--remove these
out <- na.omit(out)
out <- out %>% 
  mutate(label = ifelse(depth==1, "1.0 m", "10.0 m"))

fig6 <- ggplot(out, aes(x = horizon, y = Percentage, color = classify, fill = classify)) +
  geom_bar(position = 'fill', stat = 'identity') +
  facet_grid(label~fct_rev(variable)) +
  ylab("% of FLARE Forecasts \n Better than Null") +
  scale_color_brewer(palette = "Set3") +
  scale_fill_brewer(palette = 'Set3') +
  theme_bw() +
  theme(panel.spacing = unit(0.5, "cm")) +
  labs(fill = 'Comparison Model', x = 'Horizon (days)') +
  guides(color = FALSE)
fig6

ggsave('./figures/fig6.tiff', fig6, scale = 0.5, dpi = 300, unit = "mm", width = 335, height = 200)
