## calculate permutation entropy

library(statcomp)
library(ggpmisc)
library(tidyverse)

lake_directory <- here::here()
sim_name <- 'SUNP_fcasts_temp_DO'

d <- read.csv(file.path(lake_directory, "targets/sunp", sim_name, "sunp-targets-insitu.csv"))

d <- d %>% 
  mutate(doy = yday(time),
         year = year(time)) %>% 
  filter(doy > 246,
         depth %in% c(1, 10),
         year %in% c(2021, 2022))


PE <- plyr::ddply(d, c("depth", "year", "variable"), \(x) {
  data = x$observed
  opd = ordinal_pattern_distribution(x = data, ndemb = 4)
  pe <- permutation_entropy(opd)
  data.frame(ndemb = 4, pe = pe)
})

PE_wide <- PE %>% 
  select(-ndemb) %>% 
  pivot_wider(names_from = year, values_from = pe)

PE_fig <- ggplot(PE_wide) +
  geom_segment(aes(x=fct_rev(as.factor(depth)), xend=fct_rev(as.factor(depth)), y=`2021`, yend=`2022`), color="grey") +
  geom_point(aes(y=`2021`, x=fct_rev(as.factor(depth)), color = '2021'), size=3 ) +
  geom_point(aes(y=`2022`, x=fct_rev(as.factor(depth)), color = '2022'), size=3 ) +
  coord_flip()+
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  ylim(0.3, 1) +
  facet_wrap(~fct_rev(variable), ncol = 1) +
  theme_bw() +
  xlab('Depth (m)') +
  ylab('Permutation Entropy') +
  labs(color = 'Year')

ggsave('./figures/fig_S15.png', PE_fig, scale = 0.7, dpi = 300, unit = "mm", width = 225, height = 220)

