
library(scoringRules)
library(tidyverse)
library(ggpubr)

lake_directory <- here::here()
setwd(lake_directory)

tgts <- read.csv('./targets/sunp/SUNP_fcasts_temp_DO/sunp-targets-insitu.csv')

# set 1.5m obs of oxygen to 1.0m, convert oxy to mg/L
tgts <- tgts %>% 
  mutate(depth = ifelse(variable=='oxygen' & depth==1.5, 1.0, depth),
         observed = ifelse(variable=='oxygen', observed*32/1000, observed),
         depth = ifelse(variable=='temperature' & depth==1.5, 1.0, depth))

depths <- c(1.0, 10.0)
tgts <- tgts %>% 
  filter(depth %in% depths) %>% 
  mutate(doy = yday(time),
         year = year(time)) 

null <- tgts %>% 
  filter(year < 2021) %>% 
  group_by(depth, variable, doy) %>% 
  mutate(mean = mean(observed, na.rm = TRUE),
         sd = sd(observed, na.rm = TRUE),
         n = n()) %>% 
  filter(n > 2) %>% 
  distinct(depth, variable, doy, .keep_all = TRUE) %>% 
  dplyr::select(doy, variable, depth, mean, sd, n)

histfig <- ggplot(null, aes(as.factor(n))) +
  geom_histogram(stat = 'count') +
  facet_grid(depth~fct_rev(variable)) +
  xlab('N (obs) across all DOYs') +
  theme_bw()

climfig <- ggplot(null, aes(x = doy, y = mean, color = as.factor(depth))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean-sd, ymax = mean + sd, alpha = 0.3, fill = as.factor(depth))) +
  facet_grid(depth~fct_rev(variable), scales = 'free') +
  ylab('Climatology Prediction') +
  xlab('Day of Year') +
  labs(fill = 'Depth', 
       color = 'Depth') +
  guides(alpha = 'none') +
  theme_bw()

tempfig <- ggplot(null[null$variable=='temperature' & null$depth==1,], aes(x = doy, y = mean, color = as.factor(depth))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean-sd, ymax = mean + sd, alpha = 0.3, fill = as.factor(depth))) +
  facet_grid(depth~fct_rev(variable), scales = 'free') +
  ylab('Temperature (˚C)') +
  xlab('Day of Year') +
  guides(alpha = 'none',
         fill = 'none',
         color = 'none') +
  theme_bw()



all_clim <- ggarrange(histfig, climfig, labels = 'auto')
ggsave('./figures/figS3.png', all_clim, width = 300, height = 150, 
       units = "mm", dpi = 300, scale = 1)
