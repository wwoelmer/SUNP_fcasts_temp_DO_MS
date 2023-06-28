## calculate permutation entropy

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

###########################################################3
## go with demb = 4

PE <- plyr::ddply(d, c("depth", "year", "variable"), \(x) {
  print(head(x))
  data = x$observed
  opd = ordinal_pattern_distribution(x = data, ndemb = 4)
  pe <- permutation_entropy(opd)
  data.frame(ndemb = 4, pe = pe)
})
PE


PE_fig <- ggplot(PE, aes(x = fct_rev(as.factor(variable)), y = pe, shape = as.factor(depth), color = as.factor(year))) +
  geom_point(size = 3) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  theme_bw() +
  xlab('Year') +
  ylab('Permutation Entropy') +
  labs(color = 'Variable',
       shape = 'Depth')
PE_fig
###########################################################################
# read in fcasts and calculate % better than null

# some subsetting variables
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)
horizons <- c(1:35)
sim_name <- 'SUNP_fsed_deep_DA' # UC_analysis_2021/start_06_30
folders <- c('all_UC_fsed_deep_DA')

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
  mutate(nCRPS = 1 - (crps/crps_clim),
         year = year(datetime))

sc_clim <- na.omit(sc_clim)

### now calculate the pct better than null to compare with PE
pct_null <- plyr::ddply(sc_clim, c("variable", "depth", "year", "horizon"), function(x){
  better <- x %>% 
    filter(x$nCRPS >= 0)
  worse <- x %>% 
    filter(x$nCRPS < 0)
  pct <- nrow(better)/(nrow(better) + nrow(worse))
  return(pct)
})

pct_null$pct <- round(pct_null$V1, 2)*100

# now calculate the mean across horizons
mean_pct_null <- plyr::ddply(sc_clim, c("variable", "depth", "year"), function(x){
  better <- x %>% 
    filter(x$nCRPS >= 0)
  worse <- x %>% 
    filter(x$nCRPS < 0)
  pct <- nrow(better)/(nrow(better) + nrow(worse))
  return(pct)
})

mean_pct_null$pct <- round(mean_pct_null$V1, 2)*100


## join with PE dataframe
PE_skill <- left_join(PE, pct_null)
mean_PE_skill <- left_join(PE, mean_pct_null)

ggplot(PE_skill, aes(x = pe, y = pct, color = horizon, shape = as.factor(depth))) +
  geom_point(size = 3) +
  facet_wrap(~fct_rev(variable), scales = 'free') +
  #  geom_smooth() +
  # scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  theme_bw() +
  labs(color = 'Horizon',
       shape = 'Depth')


ggplot(mean_PE_skill, aes(x = pe, y = pct)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black', se = F) +
  geom_point(aes(shape = as.factor(depth), color = as.factor(variable)), size = 3) +
  xlab('PE') +
  ylab('% of Fcasts better than Null') +
  scale_color_manual(values =  c('darkgrey', 'tan4')) +
  theme_bw() +
  # ggplot2::annotate("text", x = 0.6, y = 0.8, label = 'p = 0.002') +
  labs(shape = 'Depth',
       color = 'Variable')
