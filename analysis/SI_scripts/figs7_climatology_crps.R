# read in climatology and calculate mean scores
library(tidyverse)

lake_directory <- here::here()

scores <- read.csv(file.path(lake_directory, 'scores/sunp/climatology_scores.csv'))

# vector of dates when obs are available (before buoy is taken into harbor)
##### make these the same dates for each year for equal comparison
buoy_dates <- c(seq.Date(as.Date('2021-08-04'), as.Date('2021-10-17'), by = 'day'),
                seq.Date(as.Date('2022-08-04'), as.Date('2022-10-17'), by = 'day'))

means <- scores %>% 
  filter(as.Date(time) %in% buoy_dates) %>% 
  group_by(variable, depth, year) %>% 
  mutate(mean_crps = mean(crps)) %>% 
  distinct(variable, year, depth, .keep_all = TRUE) %>% 
  dplyr::select(variable, depth, year, mean_crps) %>% 
  mutate(variable = factor(variable, 
                           levels = c('temperature', 'oxygen'), 
                           ordered = TRUE, 
                           labels = c('temperature (°C)', 'oxygen (mg/L)')))


f <- ggplot(means, aes(x = as.factor(year), y = mean_crps, color = as.factor(year))) +
  geom_point(size = 3) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_grid(depth~variable, scales = 'free') +
  ggtitle('Climatology Performance') +
  xlab('Year') +
  ylab('Climatology CRPS') +
  labs(color = 'Year') +
  theme_bw()

ggsave('./figures/fig_S7.png', f, dpi = 300, unit = "mm", width = 225, height = 120)
