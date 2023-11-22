# plot parameter evolution for each year during spinup

library(lubridate)
library(tidyverse)
library(ggpubr)
library(arrow)

lake_directory <- here::here()
site_id <- 'sunp'
sim_name <- 'SUNP_fcasts_temp_DO'

#######################################################################################
# read in the files
parms <- c("Fsed_oxy_zone1", "Fsed_oxy_zone2", "Fsed_oxy_zone3",
           "lw_factor", "zone1temp","zone2temp", "zone3temp")

score_dir <- file.path(lake_directory,"scores", site_id, sim_name)
fils <- list.files(score_dir, pattern = "all_UC_fsed_deep_DA_H")

df1 <- read_parquet(file.path(score_dir, fils[1]))
df2 <- read_parquet(file.path(score_dir, fils[2]))

df <- rbind(df1, df2)

df <- df %>% 
  filter(variable %in% parms)

df <- df %>% 
  mutate(doy = yday(datetime),
         year = year(datetime))

df$variable <- 
  factor(df$variable, 
         levels = c("Fsed_oxy_zone1",
                    "Fsed_oxy_zone2",
                    "Fsed_oxy_zone3",
                    "zone1temp",
                    "zone2temp",
                    "zone3temp",
                    "lw_factor")) 


fsed <- df %>% 
  filter(variable %in% c("Fsed_oxy_zone1",
                         "Fsed_oxy_zone2",
                         "Fsed_oxy_zone3")) %>% 
  ggplot(aes(x = doy, y = mean, color = as.factor(variable))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = as.factor(variable)), alpha = 0.3) +
  scale_color_manual(labels = c('zone1', 'zone2', 'zone3'), values = c('#3E517A', '#B08EA2', '#32908F')) +
  scale_fill_manual(labels = c('zone1', 'zone2', 'zone3'), values = c('#3E517A', '#B08EA2', '#32908F')) +
  facet_wrap(~year, scales = 'free_x') +
  ylab('Parameter Value') +
  xlab('Day of Year') +
  theme_bw() +
  ggtitle('Fsed at 3 zones') +
  labs(color = 'Zone',
       fill = 'Zone')


sedt <- df %>% 
  filter(variable %in% c("zone1temp",
                         "zone2temp",
                         "zone3temp")) %>% 
  ggplot(aes(x = doy, y = mean, color = as.factor(variable))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = as.factor(variable)), alpha = 0.3) +
  scale_color_manual(labels = c('zone1', 'zone2', 'zone3'), values = c('#3E517A', '#B08EA2', '#32908F')) +
  scale_fill_manual(labels = c('zone1', 'zone2', 'zone3'), values = c('#3E517A', '#B08EA2', '#32908F')) +
  facet_wrap(~year, scales = 'free_x') +
  theme_bw() +
  ylab('Parameter Value') +
  xlab('Day of Year') +
  ggtitle('Sediment Temperature at 3 zones') +
  labs(color = 'Zone',
       fill = 'Zone')


lw <- 
  df %>% 
  filter(variable %in% c("lw_factor")) %>% 
  ggplot(aes(x = doy, y = mean, color = as.factor(variable))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = as.factor(variable)), alpha = 0.3) +
  scale_color_manual(values = c('#C9ADA1')) +
  facet_wrap(~year, scales = 'free_x') +
  theme_bw() +
  ylab('Parameter Value') +
  xlab('Day of Year') +
  ggtitle('LongWave Factor')

fig <- ggarrange(fsed, sedt, lw, common.legend = TRUE)

ggsave('./figures/figS2.tiff', fig, dpi = 300, unit = "mm", width = 200, height = 150)
