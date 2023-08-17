#install.packages('ggh4x')
library(ggh4x)
PE_summ <- read.csv('./data_processed/PE.csv')
skill <- read.csv('./data_processed/skill.csv')
perf <- read.csv('./data_processed/performance.csv')

# convert performance so that higher values mean better performance, to match skill
perf$X2021 <- perf$X2021*-1
perf$X2022 <- perf$X2022*-1

dat <- rbind(perf, skill)
dat <- rbind(dat, PE_summ)
#write.csv(dat, './data_processed/all_metrics.csv')

dat_mean <- dat %>% 
  filter(metric=='Performance') %>% 
  pivot_longer(c(X2021, X2022), names_to = 'Year', values_to = 'crps') %>% 
  group_by(variable, depth) %>% 
  mutate(mean = mean(crps)) %>% 
  distinct(variable, depth, .keep_all = TRUE)
dat_mean

ggplot(dat) +
  geom_segment(aes(x=fct_rev(as.factor(depth)), xend=fct_rev(as.factor(depth)), y=X2021, yend=X2022), color="grey") +
  geom_point(aes(y=X2021, x=fct_rev(as.factor(depth)), color = '2021'), size=3 ) +
  geom_point(aes(y=X2022, x=fct_rev(as.factor(depth)), color = '2022'), size=3 ) +
  coord_flip()+
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_grid(metric~fct_rev(variable), scales = 'free') +
  theme_bw() +
  xlab('Depth') +
  ylab('Metric') +
  labs(color = 'Year')

ggplot(dat) +
  geom_segment(aes(x=fct_rev(as.factor(depth)), xend=fct_rev(as.factor(depth)), y=X2021, yend=X2022), color="grey") +
  geom_point(aes(y=X2021, x=fct_rev(as.factor(depth)), color = '2021'), size=3 ) +
  geom_point(aes(y=X2022, x=fct_rev(as.factor(depth)), color = '2022'), size=3 ) +
  coord_flip()+
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(metric~fct_rev(variable), scales = 'free', ncol = 2) +
  theme_bw() +
  theme(strip.placement = 'outside') +
  xlab('Depth') +
  ylab('Metric') +
  labs(color = 'Year')

dat |> 
  ggplot() +
  geom_segment(aes(x=fct_rev(as.factor(depth)), xend=fct_rev(as.factor(depth)), y=X2021, yend=X2022), color="grey") +
  geom_point(aes(y=X2021, x=fct_rev(as.factor(depth)), color = '2021'), size=3 ) +
  geom_point(aes(y=X2022, x=fct_rev(as.factor(depth)), color = '2022'), size=3 ) +
  coord_flip()+
  scale_color_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_grid2(metric~fct_rev(variable), independent = T, scales = 'free') +
  theme_bw() +
  xlab('Depth') +
  ylab('Value') +
  labs(color = 'Year')
