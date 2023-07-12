# calculate long-term values of TP, TN, chla, and secchi from LMP dataset

library(tidyverse)
lake_directory <- here::here()

dat <- read.csv(file.path(lake_directory, 'data_raw/hist-data/LSPA_LMP/LSPALMP_1986-2022_v2023-06-04.csv'))
parms <- c('phosphorusTotal_mgl',
           'chlorophyll_a_ugl',
           'secchiDepth_m')

dat <- dat %>% 
  filter(parameter %in% parms,
         site_type=='lake',
         station==210)

means <- plyr::ddply(dat, c("parameter"), function(x){
  mean(x$value, na.rm = TRUE)
})

means

