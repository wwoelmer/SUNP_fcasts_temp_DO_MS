## calculate permutation entropy

library(statcomp)
library(ggpmisc)

lake_directory <- here::here()
sim_name <- 'SUNP_fcasts_temp_DO'

d <- read.csv(file.path(lake_directory, "targets/sunp", sim_name, "sunp-targets-insitu.csv"))

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

ggplot(PE, aes(x = as.factor(year), y = pe, color = as.factor(year))) +
  geom_point(size = 3) +
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

PE_fig <- ggplot(PE_wide) +
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

ggsave('./figures/fig_S5_PE_obs.png', PE_fig, scale = 0.7, dpi = 300, unit = "mm", width = 225, height = 220)

