# read in climatology and calculate mean scores
library(tidyverse)

lake_directory <- here::here()

scores <- read.csv(file.path(lake_directory, 'scores/sunp/climatology_scores.csv'))

means <- scores %>% 
  filter(as.Date(time) %in% buoy_dates) %>% 
  group_by(variable, depth, year) %>% 
  mutate(mean_crps = mean(crps),
         mean_log = mean(logs)) %>% 
  distinct(variable, year, depth, .keep_all = TRUE) %>% 
  dplyr::select(variable, depth, year, mean_crps:mean_log)
means$variable <- factor(means$variable, levels = c('temperature', 'oxygen'), 
                         ordered = TRUE, labels = c('temperature (C)', 'oxygen (mg/L)'))

ggplot(means, aes(x = mean_crps, y = mean_log)) +
  geom_point()

f <- ggplot(means, aes(x = as.factor(year), y = mean_crps, color = as.factor(year))) +
  geom_point(size = 3) +
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_grid(depth~variable, scales = 'free') +
  ggtitle('Climatology Performance') +
  xlab('Year') +
  ylab('Climatology CRPS') +
  labs(color = 'Year') +
  theme_bw()

ggsave('./figures/fig_S6_climatology_crps.png', f, dpi = 300, unit = "mm", width = 225, height = 120)
