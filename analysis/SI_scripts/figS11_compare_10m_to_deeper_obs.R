
library(tidyverse)
library(ggpmisc)
library(ggpubr)

lake_directory <- here::here()

# download data
# manually collected data
if(!file.exists(file.path(lake_directory, 'data_raw', 'hist-data', 'LMP-v2023.1.zip'))){
  download.file(url = 'https://zenodo.org/records/8003784/files/Lake-Sunapee-Protective-Association/LMP-v2023.2.zip?download=1',
                destfile = file.path(lake_directory, 'data_raw', 'hist-data', 'LMP-v2023.1.zip'),
                mode = 'wb')
  unzip(file.path(lake_directory, 'data_raw', 'hist-data', 'LMP-v2023.1.zip'),
        files = file.path('Lake-Sunapee-Protective-Association-LMP-42d9cc5', 'primary files', 'LSPALMP_1986-2022_v2023-06-04.csv'),
        exdir = file.path(lake_directory, 'data_raw', 'hist-data', 'LSPA_LMP'),
        junkpaths = TRUE)
}

# read in deep site data
lmp <- read.csv('./data_raw/hist-data/LSPA_LMP/LSPALMP_1986-2022_v2023-06-04.csv') %>% 
  dplyr::filter(parameter %in% c("waterTemperature_degC", "oxygenDissolved_mgl", "oxygenDissolvedPercentOfSaturation_pct")) %>% 
  dplyr::mutate(date = as.Date(date)) %>% 
  dplyr::select(date, depth_m, parameter, value, station) %>% 
  tidyr::pivot_wider(names_from = parameter, values_from = value) %>% 
  tidyr::unchop(everything()) %>% 
  filter(station==210) %>% 
  rename(temp_C = waterTemperature_degC,
         DO_mgl = oxygenDissolved_mgl,
         DO_pctsat = oxygenDissolvedPercentOfSaturation_pct) %>% 
  mutate(depth = round(depth_m))

a <- lmp %>% 
  filter(depth %in% c(10, 15, 20, 30)) %>% 
  ggplot(aes(x = date, y = DO_mgl, color = as.factor(depth))) +
  geom_line() +
  theme_bw() +
  ylab('Oxygen (mg/L') +
  labs(color = 'Depth')

lmp_wide <-  lmp %>%  
  distinct(depth, date, .keep_all = TRUE) %>% 
  dplyr::select(date, DO_mgl, depth) %>% 
  filter(depth %in% c(10, 15, 20, 30)) %>% 
  pivot_wider(names_from = depth, values_from = DO_mgl, names_prefix = "d_") 


b <- ggplot(lmp_wide, aes(x = d_10, y = d_15, color = '15 and 10')) +
  geom_point() +
  geom_smooth(aes(x = d_10, y = d_15, color = '15 and 10'), method = 'lm') +
  #stat_poly_eq() +
  geom_point(data = lmp_wide, aes(x = d_10, y = d_20, color = '20 and 10')) +
  geom_smooth(aes(x = d_10, y = d_20, color = '20 and 10'), method = 'lm') +
  #stat_poly_eq(aes(x = d_10, y = d_20, color = '20 and 10')) +
  geom_point(data = lmp_wide, aes(x = d_10, y = d_30, color = '30 and 10')) +
  geom_smooth(aes(x = d_10, y = d_30, color = '30 and 10'), method = 'lm') +
  #stat_poly_eq(aes(x = d_10, y = d_30, color = '30 and 10')) +
  ylab('Deeper observations') +
  xlab('10 m observations') +
  theme_bw() +
  labs(color = 'Depths (m)')
  

fig <- ggarrange(a, b)
fig
ggsave('./figures/fig_S12.png', fig, scale = 0.5, dpi = 300, unit = "mm", width = 475, height = 220)
