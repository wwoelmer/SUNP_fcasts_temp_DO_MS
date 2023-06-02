## calculate permutation entropy

library(statcomp)
library(ggpmisc)

d <- read.csv(file.path(lake_directory, "targets/sunp/SUNP_fsed_deep_DA/sunp-targets-insitu.csv"))

d <- d %>% 
  mutate(doy = yday(time),
         year = year(time)) %>% 
  filter(doy > 246,
         depth %in% c(1, 10),
         year %in% c(2021, 2022))
  

o_10_21 <- d %>% 
  filter(depth==10,
         variable=="oxygen",
         year==2022)

x = o_10_21$observed
opd = ordinal_pattern_distribution(x = x, ndemb = 3)
permutation_entropy(opd)

df <- matrix(ncol = 5, nrow = 10)
colnames(df) <- c('year', 'variable', 'depth', 'PE', 'demb')
        
out <- plyr::ddply(d, c("depth", "year", "variable"), \(x) {
  print(head(x))
  data = x$observed
  pe <- sapply(3:10, \(i) {
    opd = ordinal_pattern_distribution(x = data, ndemb = i)
    permutation_entropy(opd)
  })
  data.frame(ndemb = 3:10, pe = pe)
})
out           

ggplot(out, aes(x = ndemb, y = pe, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  facet_grid(depth ~ fct_rev(variable)) +
  theme_bw() +
  xlab('Embedding Distance') +
  ylab('Permutation Entropy') +
  labs(color = 'Year')

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


ggplot(PE, aes(x = as.factor(year), y = pe, color = as.factor(year), shape = as.factor(variable))) +
  geom_point(size = 3) +
  #geom_jitter(size = 3) +
  #ylim(0.5, 0.9) +
  scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  facet_wrap(~depth, ncol = 1) +
  theme_bw() +
  xlab('Year') +
  ylab('Permutation Entropy') +
  labs(color = 'Year',
       shape = 'Variable')

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

mean_crps <- sc %>% 
  filter(depth %in% c(1.0, 10.0)) %>% 
  group_by(year, depth, variable) %>% 
  dplyr::summarise(mean = mean(crps, na.rm = TRUE), 
                   median = median(crps, na.rm = TRUE),
                   min = min(crps, na.rm = TRUE),
                   max = max(crps, na.rm = TRUE),
                   range = abs(min - max),
                   sd = sd(crps, na.rm = TRUE),
                   cv = sd(crps, na.rm = TRUE)/mean)

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

ggplot(both, aes(x = pe, y = sd, group = horizon, color = horizon, shape = as.factor(depth))) +
  geom_smooth(method = 'lm') +
  geom_point() +
  facet_wrap(~fct_rev(variable), scales = 'free') +
  #scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  theme_bw() +
  labs(color = 'Horizon',
       shape = 'Depth') +
  ggtitle('SD across skill')

ggplot(both, aes(x = pe, y = range, group = horizon, color = horizon, shape = as.factor(depth))) +
  geom_smooth(method = 'lm') +
  geom_point() +
  facet_wrap(~fct_rev(variable), scales = 'free') +
  #scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  theme_bw() +
  labs(color = 'Horizon',
       shape = 'Depth') +
  ggtitle('Range across skill')

ggplot(both, aes(x = pe, y = median, group = horizon, color = horizon, shape = as.factor(depth))) +
  geom_smooth(method = 'lm') +
  geom_point() +
  facet_wrap(~fct_rev(variable), scales = 'free') +
  #scale_color_manual(values =  c('#17BEBB', '#9E2B25')) +
  theme_bw() +
  #stat_fit_glance(method = 'lm',
  #                method.args = list(formula = y ~ x),  geom = 'text', 
  #                aes(label = paste("p-value=", signif(..p.value.., digits = 4))),
  #                size = 5) +
  labs(color = 'Horizon',
       shape = 'Depth') +
  ggtitle('Median skill')

lm_df <- both %>% 
  select(pe, variable, median, range, sd) %>% 
  pivot_longer(c(median, range, sd),
               names_to = 'summ_var',
               values_to = 'value')


lm_out <- plyr::ddply(lm_df, c("summ_var", "variable"), \(x) {
  print(head(x))
  data = x
  lm <- lm(data$pe ~ data$value)
  p <- round(summary(lm)$coefficients[8], 3)
  data.frame(p = p)
})

lm_out

PE <- plyr::ddply(d, c("depth", "year", "variable"), \(x) {
  print(head(x))
  data = x$observed
  opd = ordinal_pattern_distribution(x = data, ndemb = 4)
  pe <- permutation_entropy(opd)
  data.frame(ndemb = 4, pe = pe)
})


lmo_sd <- lm(pe~sd, data = both[both$variable=='oxygen',])
summary(lmo_sd)
names(summary(lmo_median))

lmt_sd <- lm(pe~sd, data = both[both$variable=='temperature',])
summary(lmt_sd)
##############################################################################################

PE_fcast <- plyr::ddply(sc, c("depth", "year", "variable"), \(x) {
  print(head(x))
  data = x$crps
  pe <- sapply(4, \(i) {
    opd = ordinal_pattern_distribution(x = data, ndemb = i)
    permutation_entropy(opd)
  })
  data.frame(ndemb = 4, pe = pe)
})

ggplot(PE_fcast, aes(x = as.factor(year), y = pe, color = 'fcast')) +
  geom_point() +
  geom_point(data = PE, aes(x = as.factor(year), y = pe, color = 'obs')) +
  facet_grid(depth ~ fct_rev(variable)) +
  theme_bw() 

pe_both <- left_join(PE, PE_fcast)
ggplot()
  