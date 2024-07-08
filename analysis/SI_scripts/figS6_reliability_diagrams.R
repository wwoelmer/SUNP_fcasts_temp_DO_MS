library(tidyverse)
#install.packages('ggpubr')
library(ggpubr)
library(arrow)

lake_directory <- here::here()

# some subsetting variables
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)
horizons <- c(1:35)
sim_name <- 'SUNP_fcasts_temp_DO' 
#folders <- c('initial_condition')

########################################################################
# read in the scores and calculate variance
score_dir <- arrow::SubTreeFileSystem$create(file.path(lake_directory,"scores/sunp", sim_name))

sc <- arrow::open_dataset(score_dir) |> 
  filter(variable %in% vars,
         depth %in% depths) %>% 
  collect() 


# vector of dates when obs are available (before buoy is taken into harbor)
##### subset to the same time frame for each year for equal comparison
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
         sd = ifelse(variable=='temperature', sd, (sd*32/1000)),
         observation = ifelse(variable=='temperature', observation, (observation*32/1000)))


##########################################################################
# reliability diagrams
x <- sc %>% 
  filter(variable == "oxygen" & depth == 1 & horizon == 3)
out <- plyr::ddply(sc, c("variable", "depth", "horizon", "year"), \(x){
  print(head(x))
  dat <- data.frame(variable = x$variable[1],
                    depth = x$depth[1],
                    horizon = x$horizon[1],
    bin = seq(10, 90, by = 10),
                    obs_freq = NA,
                    low = c(0.45, 0.40, 0.35, 0.3, 0.25, 0.20, 0.15, 0.10, 0.05),
                    high = c(0.55, 0.60, 0.65, 0.7, 0.75, 0.80, 0.85, 0.9, 0.95),
                    UC = 'all_UC_fsed_deep_DA')
  
  # temp <- sc %>% 
  #   select(reference_datetime, datetime, depth, model_id, variable, horizon,
  #          observation, mean, sd, doy, year) %>% 
  #   filter(!is.na(observation),
  #          variable== x$variable[1], # all grouping variables
  #          horizon== x$horizon[1],   
  #          depth== x$depth[1])
  vec <- c()

  for(j in 1:length(unique(dat$bin))){
    
    for(i in 1:nrow(x)){
      # subset to forecasts within each break range of the distribution
      prob_above <- qnorm(dat$high[j], x$mean[i], x$sd[i])
      prob_below <- qnorm(dat$low[j], x$mean[i], x$sd[i])
     
      if (is.na(x$observation[i])) {
        vec <- c(vec, 0)
      } else  if(x$observation[i] < prob_above & x$observation[i] > prob_below){
        # 1 if obs in in CI, 0 if not
        vec <- c(vec, 1)
      } else {
        vec <- c(vec, 0)
      }
    }
    dat$obs_freq[j] <- sum(vec)/nrow(x)*100
    vec <- c()
  }
  
  return(dat)
})

rel_p <- ggplot(out, aes(x = bin, y = bin, color = as.factor(horizon))) +
  geom_line(aes(y = obs_freq, color = as.factor(horizon)), size = 1) +
  geom_point(color = 'black') +
  facet_wrap(depth ~ fct_rev(variable)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + # Set x-axis ticks every 10 units
  xlab('Forecast confidence interval (%)') +
  ylab('Percent of observations within forecast interval') +
  labs(color = 'Forecast Horizon (days)') +
  theme(text = element_text(size = 14))
  
out %>% 
  filter(depth==1) %>% 
ggplot(aes(x = bin, y = bin, color = as.factor(horizon))) +
  geom_line(aes(y = obs_freq, color = as.factor(horizon)), size = 1) +
  geom_line(color = 'black') +
  facet_wrap(fct_rev(variable) ~ year) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + # Set x-axis ticks every 10 units
  xlab('Forecast confidence interval (%)') +
  ylab('Percent of observations within forecast interval') +
  labs(color = 'Forecast Horizon (days)') +
  theme(text = element_text(size = 14)) +
  ggtitle('1.0 m forecasts')


out %>% 
  filter(depth==10) %>% 
  ggplot(aes(x = bin, y = bin, color = as.factor(horizon))) +
  geom_line(aes(y = obs_freq, color = as.factor(horizon)), size = 1) +
  geom_line(color = 'black') +
  facet_wrap(fct_rev(variable) ~ year) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + # Set x-axis ticks every 10 units
  xlab('Forecast confidence interval (%)') +
  ylab('Percent of observations within forecast interval') +
  labs(color = 'Forecast Horizon (days)') +
  theme(text = element_text(size = 14)) +
  ggtitle('10.0 m forecasts')


rel_p

ggsave('./figures/figs6_reliability_plot.tiff', rel_p, scale = 0.6, dpi = 300, 
       unit = "mm", width = 450, height = 500)
