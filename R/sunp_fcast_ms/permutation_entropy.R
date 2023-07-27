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

#df <- matrix(ncol = 5, nrow = 10)
#colnames(df) <- c('year', 'variable', 'depth', 'PE', 'demb')
        
#out <- plyr::ddply(d, c("depth", "year", "variable"), \(x) {
#  print(head(x))
#  data = x$observed
#  pe <- sapply(3:10, \(i) {
#    opd = ordinal_pattern_distribution(x = data, ndemb = i)
#    permutation_entropy(opd)
#  })
#  data.frame(ndemb = 3:10, pe = pe)
#})
#out           

#ggplot(out, aes(x = ndemb, y = pe, color = as.factor(year))) +
#  geom_line() +
#  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
#  facet_grid(depth ~ fct_rev(variable)) +
#  theme_bw() +
#  xlab('Embedding Distance') +
#  ylab('Permutation Entropy') +
#  labs(color = 'Year')

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


ggplot(PE, aes(x = as.factor(year), y = pe, shape = as.factor(depth), color = as.factor(variable))) +
  geom_point(size = 3) +
  #geom_jitter(size = 3) +
  #ylim(0.5, 0.9) +
  scale_color_manual(values =  c('darkgrey', 'tan4')) +
  #facet_wrap(~depth, ncol = 1) +
  theme_bw() +
  xlab('Year') +
  ylab('Permutation Entropy') +
  labs(color = 'Variable',
       shape = 'Depth')

ggplot(PE, aes(x = as.factor(year), y = pe, color = as.factor(year))) +
  geom_point(size = 3) +
  #geom_jitter(size = 3) +
  #ylim(0.5, 0.9) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_grid(depth~fct_rev(variable)) +
  theme_bw() +
  xlab('Year') +
  ylab('Permutation Entropy') +
  labs(color = 'Variable',
       shape = 'Depth')

PE_wide <- PE %>% 
  select(-ndemb) %>% 
  pivot_wider(names_from = year, values_from = pe)

ggplot(PE_wide) +
  geom_segment(aes(x=fct_rev(as.factor(depth)), xend=fct_rev(as.factor(depth)), y=`2021`, yend=`2022`), color="grey") +
  geom_point(aes(y=`2021`, x=fct_rev(as.factor(depth)), color = '2021'), size=3 ) +
  geom_point(aes(y=`2022`, x=fct_rev(as.factor(depth)), color = '2022'), size=3 ) +
  coord_flip()+
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  ylim(0.3, 1) +
  facet_wrap(~fct_rev(variable), ncol = 1) +
  theme_bw() +
  xlab('Depth') +
  ylab('Permutation Entropy') +
  labs(color = 'Year')

PE_wide$metric <- 'PE'

write.csv(PE_wide, './data_processed/PE.csv', row.names = FALSE)



################################################################################################################
## compare to weighted PE

PE_w <- plyr::ddply(d, c("depth", "year", "variable"), \(x) {
  print(head(x))
  data = x$observed
  opd =weighted_ordinal_pattern_distribution(x = data, ndemb = 4)
  pe <- permutation_entropy(opd)
  data.frame(ndemb = 4, pe = pe)
})
PE_w

PE_wide_w <- PE_w %>% 
  select(-ndemb) %>% 
  pivot_wider(names_from = year, values_from = pe) %>% 
  mutate(method = 'w_PE')

ggplot(PE_wide_w) +
  geom_segment(aes(x=fct_rev(as.factor(depth)), xend=fct_rev(as.factor(depth)), y=`2021`, yend=`2022`), color="grey") +
  geom_point(aes(y=`2021`, x=fct_rev(as.factor(depth)), shape = method, color = '2021'), size=3 ) +
  geom_point(aes(y=`2022`, x=fct_rev(as.factor(depth)), color = '2022', shape = method), size=3 ) +
  coord_flip()+
  ylim(0.3, 1) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~fct_rev(variable), ncol = 1) +
  theme_bw() +
  xlab('Depth') +
  ylab('Weighted Permutation Entropy') +
  labs(color = 'Year')

PE_wide <- PE_wide %>% 
  mutate(method = 'PE')

PE_compare <- full_join(PE_wide, PE_wide_w)

ggplot(PE_compare) +
  geom_segment(aes(x=fct_rev(as.factor(depth)), xend=fct_rev(as.factor(depth)), y=`2021`, yend=`2022`), color="grey") +
  geom_point(aes(y=`2021`, x=fct_rev(as.factor(depth)), shape = method, color = '2021'), size=3 ) +
  geom_point(aes(y=`2022`, x=fct_rev(as.factor(depth)), color = '2022', shape = method), size=3 ) +
  coord_flip()+
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~fct_rev(variable), ncol = 1) +
  theme_bw() +
  xlab('Depth') +
  ylab('Weighted Permutation Entropy') +
  labs(color = 'Year')

##############################################################################################################


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
## calculate PE on fcast time series?
lake_directory <- here::here()

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
  mutate(nCRPS = (crps/crps_clim),
         year = year(datetime))

sc_clim <- na.omit(sc_clim)

summ_crps <- sc_clim %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(year, depth, variable, horizon) %>% 
  dplyr::summarise(mean = mean(nCRPS, na.rm = TRUE), 
                   median = median(nCRPS, na.rm = TRUE),
                   min = min(nCRPS, na.rm = TRUE),
                   max = max(nCRPS, na.rm = TRUE),
                   range = abs(min - max),
                   sd = sd(nCRPS, na.rm = TRUE),
                   cv = sd(nCRPS, na.rm = TRUE)/mean)

mean_crps <- sc_clim %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(year, depth, variable) %>% 
  dplyr::summarise(mean = mean(nCRPS, na.rm = TRUE), 
                   median = median(nCRPS, na.rm = TRUE),
                   min = min(nCRPS, na.rm = TRUE),
                   max = max(nCRPS, na.rm = TRUE),
                   range = abs(min - max),
                   sd = sd(nCRPS, na.rm = TRUE),
                   cv = sd(nCRPS, na.rm = TRUE)/mean)

both <- left_join(PE, summ_crps)
both_mean <- left_join(PE, mean_crps)

ggplot(both, aes(x = pe, y = median, color = horizon, shape = as.factor(depth))) +
  geom_point(size = 3) +
  facet_wrap(~fct_rev(variable), scales = 'free') +
  #  geom_smooth() +
  # scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  theme_bw() +
  labs(color = 'Horizon',
       shape = 'Depth')



###################################################################################################
#######################################################################################################
#### OLD
sc <- sc %>% 
  mutate(doy = yday(datetime),
         year = year(datetime)) %>% 
  mutate(crps = ifelse(variable=='temperature', crps, (crps*32/1000))) %>% 
  select(-c(family, site_id)) %>% 
  filter(horizon > 0,
         as.Date(reference_datetime) %in% buoy_dates) %>% 
  select(model_id, reference_datetime, datetime, horizon, depth, variable, everything()) 

summ_crps <- sc %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(year, depth, variable, horizon) %>% 
  dplyr::summarise(mean = mean(crps, na.rm = TRUE), 
                   median = median(crps, na.rm = TRUE),
                   min = min(crps, na.rm = TRUE),
                   max = max(crps, na.rm = TRUE),
                   range = abs(min - max),
                   sd = sd(crps, na.rm = TRUE),
                   cv = sd(crps, na.rm = TRUE)/mean)

mean_crps <- sc_clim %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(year, depth, variable) %>% 
  dplyr::summarise(mean = mean(nCRPS, na.rm = TRUE), 
                   median = median(nCRPS, na.rm = TRUE),
                   min = min(nCRPS, na.rm = TRUE),
                   max = max(nCRPS, na.rm = TRUE),
                   range = abs(min - max),
                   sd = sd(nCRPS, na.rm = TRUE),
                   cv = sd(nCRPS, na.rm = TRUE)/mean)

both <- left_join(PE, summ_crps)
both_mean <- left_join(PE, mean_crps)

ggplot(both, aes(x = pe, y = median, color = horizon, shape = as.factor(depth))) +
  geom_point(size = 3) +
  facet_wrap(~fct_rev(variable), scales = 'free') +
#  geom_smooth() +
 # scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  theme_bw() +
  labs(color = 'Horizon',
       shape = 'Depth')

ggplot(both, aes(x = pe, y = median, group = pe)) +
  geom_jitter(size = 3, aes(color = horizon, shape = as.factor(depth))) +
  geom_boxplot(alpha = 0.7)  +
  facet_wrap(~fct_rev(variable), scales = 'free') 
  

# calculate linear model
lm_df <- both %>% 
  select(pe, variable, median, range, sd) %>% 
  pivot_longer(c(median, range, sd),
               names_to = 'summ_var',
               values_to = 'value')

lm_median <- lm_df %>%
  filter(summ_var=='median')
summary(lm(lm_median$pe ~ lm_median$value))


lm_out <- plyr::ddply(lm_df, c("summ_var"), \(x) {
  print(head(x))
  data = x
  lm <- lm(data$pe ~ data$value)
  p <- round(summary(lm)$coefficients[8], 3)
  data.frame(p = p)
})

lm_out


all <- ggplot(both_mean, aes(x = pe, y = median)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black', se = F) +
  geom_point(aes(shape = as.factor(depth), color = as.factor(variable)), size = 3) +
  xlab('PE') +
  ylab('nCRPS') +
  scale_color_manual(values =  c('darkgrey', 'tan4')) +
  theme_bw() +
 # ggplot2::annotate("text", x = 0.6, y = 0.8, label = 'p = 0.002') +
  labs(shape = 'Depth',
       color = 'Variable')
all

######################################################
# pe vs metrics of forecast performance
ggplot(both, aes(x = pe, y = sd, group = horizon, color = horizon, shape = as.factor(depth))) +
  geom_smooth(method = 'lm', se = F) +
  geom_point() +
  #geom_line() +
  facet_wrap(~fct_rev(variable), scales = 'free') +
  #scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  theme_bw() +
  labs(color = 'Horizon',
       shape = 'Depth') +
  ggtitle('SD across skill')

ggplot(both, aes(x = pe, y = median, group = horizon, color = horizon, shape = as.factor(depth))) +
  geom_smooth(method = 'lm', se = F) +
  #geom_line() +
  geom_point() +
  facet_wrap(~fct_rev(variable), scales = 'free') +
  #scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  theme_bw() +
  labs(color = 'Horizon',
       shape = 'Depth') +
  ggtitle('Median skill')


med_bp <- ggplot(both, aes(x = pe, y = median, group = pe)) +
  geom_boxplot(width = 0.05, outlier.shape = NA) +
  geom_jitter(aes(x = pe, y = median, group = horizon, color = horizon, shape = as.factor(depth))) +
  facet_wrap(~fct_rev(variable), scales = 'free') +
  scale_fill_manual(values =  c('#17BEBB', '#9E2B25')) +
  theme_bw() +
  ylab('Median CRPS') +
  xlab('PE') +
  labs(color = 'Horizon',
       shape = 'Depth',
       fill = 'Year')
med_bp

ggarrange(all, med_bp, labels = 'auto')



PE <- plyr::ddply(d, c("depth", "year", "variable"), \(x) {
  print(head(x))
  data = x$observed
  opd = ordinal_pattern_distribution(x = data, ndemb = 4)
  pe <- permutation_entropy(opd)
  data.frame(ndemb = 4, pe = pe)
})
PE


##############################################################################################
## calculate PE of the forecast prediction
PE_fcast <- plyr::ddply(sc, c("depth", "year", "variable", "horizon"), \(x) {
  print(head(x))
  data = x$median
  pe <- sapply(4, \(i) {
    opd = ordinal_pattern_distribution(x = data, ndemb = i)
    permutation_entropy(opd)
  })
  data.frame(ndemb = 4, pe = pe)
})

ggplot(PE_fcast, aes(x = as.factor(year), y = pe, color = as.factor(horizon))) +
  geom_point() +
  #geom_point(data = PE, aes(x = as.factor(year), y = pe, color = 'obs')) +
  facet_grid(depth ~ fct_rev(variable)) +
  theme_bw() 

ggplot(PE_fcast, aes(x = horizon, y = pe, color = as.factor(year))) +
  geom_point() +
  geom_hline(data = PE, aes(color = as.factor(year), yintercept = pe), size = 1) +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  facet_grid(depth ~ fct_rev(variable)) +
  theme_bw() +
  ylab('PE of fcast at each horizon') +
  labs(color = 'Year') +
  guides(size = FALSE)

## recreate the PE obs figure
ggplot(PE_fcast, aes(x = as.factor(year), y = pe, color = (horizon), shape = as.factor(variable))) +
  geom_jitter(size = 3) +
  #scale_color_manual(values =  c('green', 'orange')) +
  facet_wrap(~depth, ncol = 1) +
  theme_bw() +
  xlab('Year') +
  ylab('Permutation Entropy') +
  labs(color = 'Horizon',
       shape = 'Variable')

##############################################################################################
## calculate PE of CRPS over time
PE_crps <- plyr::ddply(sc, c("depth", "year", "variable", "horizon"), \(x) {
  print(head(x))
  data = x$crps
  pe <- sapply(4, \(i) {
    opd = ordinal_pattern_distribution(x = data, ndemb = i)
    permutation_entropy(opd)
  })
  data.frame(ndemb = 4, pe = pe)
})


ggplot(PE_crps, aes(x = horizon, y = pe, color = as.factor(year))) +
  geom_line() +
  geom_smooth() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  facet_grid(depth ~ fct_rev(variable)) +
  theme_bw() +
  ylab('PE of CRPS at each horizon') +
  labs(color = 'Year',
       shape = ' ') +
  guides(size = FALSE)

## recreate the PE obs figure
ggplot(PE_fcast, aes(x = as.factor(year), y = pe, color = (horizon), shape = as.factor(variable))) +
  geom_jitter(size = 3) +
  #scale_color_manual(values =  c('green', 'orange')) +
  facet_wrap(~depth, ncol = 1) +
  theme_bw() +
  xlab('Year') +
  ylab('Permutation Entropy') +
  labs(color = 'Horizon',
       shape = 'Variable')



###########################################################################################

###########################################################################################
## add in climatology scores to compare to PE
scores <- read.csv(file.path(lake_directory, 'scores/sunp/climatology_scores.csv'))

means <- scores %>% 
  filter(as.Date(time) %in% buoy_dates) %>% 
  group_by(variable, depth, year) %>% 
  mutate(mean_crps = mean(crps),
         sd_crps = sd(crps),
         range_crps = abs(min(crps) - max(crps))) %>% 
  distinct(variable, year, depth, .keep_all = TRUE) %>% 
  dplyr::select(variable, depth, year, mean_crps:range_crps)

PE_null <- left_join(PE, means)


ggplot(PE_null, aes(x = pe, y = mean_crps, color = as.factor(year), shape = as.factor(depth))) +
  geom_point(size = 3) +
  facet_wrap(~fct_rev(variable), scales = 'free') +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  theme_bw() +
  labs(color = 'Horizon',
       shape = 'Depth') +
  ggtitle('Mean skill of null')

ggplot(PE_null, aes(x = pe, y = sd_crps, color = as.factor(year), shape = as.factor(depth))) +
  geom_point(size = 3) +
  facet_wrap(~fct_rev(variable), scales = 'free') +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  theme_bw() +
  labs(color = 'Horizon',
       shape = 'Depth') +
  ggtitle('SD of null skill')

ggplot(PE_null, aes(x = pe, y = range_crps, color = as.factor(year), shape = as.factor(depth))) +
  geom_point(size = 3) +
  facet_wrap(~fct_rev(variable), scales = 'free') +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  theme_bw() +
  labs(color = 'Horizon',
       shape = 'Depth') +
  ggtitle('Range of null skill')



ggplot(PE_fcast, aes(x = as.factor(year), y = pe, color = 'fcast')) +
  geom_point() +
  geom_point(data = PE, aes(x = as.factor(year), y = pe, color = 'obs')) +
  geom_point(data = mean, aes(x = as.factor(year), y = pe, color = 'obs')) +
  facet_grid(depth ~ fct_rev(variable)) +
  theme_bw() 
