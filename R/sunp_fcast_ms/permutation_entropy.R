## calculate permutation entropy

library(statcomp)

d <- read.csv(file.path(lake_directory, "targets/sunp/UC_analysis_2022/sunp-targets-insitu.csv"))

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
