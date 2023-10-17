# script to create targets file (observations) 
# and generate climatology forecast from historical observations

library(scoringRules)
library(tidyverse)
library(ggpubr)
library(FLAREr)

lake_directory <- here::here()
forecast_site <- "sunp"
configure_run_file <- "configure_run.yml"
config_files <- "configure_flare.yml"
config_set_name <- "SUNP_fcasts_temp_DO"
use_archive <- FALSE

setwd(lake_directory)

###########################################################
message("Generating targets")
source(file.path(lake_directory, "R", "insitu_qaqc_withDO.R"))

#' Generate the `config_obs` object and create directories if necessary
message('read config')
config_obs <- FLAREr::initialize_obs_processing(lake_directory, observation_yml = "observation_processing.yml", config_set_name = config_set_name)
dir.create(file.path(lake_directory, "targets", config_obs$site_id, config_set_name), showWarnings = FALSE)

#' Clone or pull from data repositories
message('download git')
FLAREr::get_git_repo(lake_directory,
                     directory = config_obs$realtime_insitu_location,
                     git_repo = "https://github.com/FLARE-forecast/SUNP-data.git")

#' Download files from EDI and Zenodo
dir.create(file.path(config_obs$file_path$data_directory, "hist-data"),showWarnings = FALSE)

# high frequency buoy data
message('download edi')
FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/499/2/f4d3535cebd96715c872a7d3ca45c196",
                     file = file.path("hist-data", "hist_buoy_do.csv"),
                     lake_directory)

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/499/2/1f903796efc8d79e263a549f8b5aa8a6",
                     file = file.path("hist-data", "hist_buoy_temp.csv"),
                     lake_directory)

# manually collected data
if(!file.exists(file.path(lake_directory, 'data_raw', 'hist-data', 'LMP-v2023.1.zip'))){
  download.file(url = 'https://zenodo.org/record/8003784/files/Lake-Sunapee-Protective-Association/LMP-v2023.2.zip?download=1',
                destfile = file.path(lake_directory, 'data_raw', 'hist-data', 'LMP-v2023.1.zip'),
                mode = 'wb')
  unzip(file.path(lake_directory, 'data_raw', 'hist-data', 'LMP-v2023.1.zip'),
        files = file.path('Lake-Sunapee-Protective-Association-LMP-42d9cc5', 'primary files', 'LSPALMP_1986-2022_v2023-06-04.csv'),
        exdir = file.path(lake_directory, 'data_raw', 'hist-data', 'LSPA_LMP'),
        junkpaths = TRUE)
}

# QAQC insitu buoy data
message('run insitu qaqc')
if(!file.exists(file.path(config_obs$file_path$targets_directory, config_obs$site_id, config_set_name))){
  dir.create(file.path(config_obs$file_path$targets_directory, config_obs$site_id, config_set_name))
}
cleaned_insitu_file <- insitu_qaqc(realtime_file = file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[1]),
                                   hist_buoy_file = c(file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[2]), file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[5])),
                                   hist_manual_file = file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[3]),
                                   hist_all_file =  file.path(config_obs$file_path$data_directory, config_obs$insitu_obs_fname[4]),
                                   maintenance_url = "https://docs.google.com/spreadsheets/d/1IfVUlxOjG85S55vhmrorzF5FQfpmCN2MROA_ttEEiws/edit?usp=sharing",
                                   variables = c("temperature", "oxygen"),
                                   cleaned_insitu_file = file.path(config_obs$file_path$targets_directory, config_obs$site_id, config_set_name, paste0(config_obs$site_id,"-targets-insitu.csv")),
                                   config = config_obs,
                                   lake_directory = lake_directory)
message("Successfully generated targets")



tgts <- read.csv('./targets/sunp/sunp-targets-insitu.csv')

ggplot(tgts, aes(x = depth)) +
  geom_histogram() +
  facet_wrap(~variable)

ggplot(tgts, aes(x = as.Date(time), y = observed)) +
  geom_line() +
  facet_grid(cols = vars(variable), rows = vars(depth), scale = 'free') 

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


tgts %>% 
  filter(variable=="temperature",
         depth==1) %>% 
ggplot(aes(x = doy, y = observed, color = as.factor(year))) +
  geom_line()

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
tempfig
ggsave('./figures/temp_climatology.png', tempfig, width = 300, height = 200, 
       units = "mm", dpi = 300, scale = 0.4)

ggarrange(histfig, climfig, labels = 'auto')

t_2021 <- tgts %>% 
  filter(year==2021,
         time > as.Date('2021-06-07')) #doy > 218 & doy < 292
t_2021 <- full_join(t_2021, null)
t_2021 <- t_2021 %>% 
  filter(observed > 0,
         sd > 0) %>% 
  group_by(depth, variable, doy) %>% 
  mutate(crps = crps.numeric(y = observed, family = "normal", 
                              mean = mean, sd = sd),
         logs = logs.numeric(y = observed, family = "normal", 
                             mean = mean, sd = sd),
         rmse = Metrics::rmse(observed, mean))

ggplot(t_2021[t_2021$variable=='oxygen',], aes(x = doy, y = crps)) +
  geom_line() +
  facet_wrap(~depth, scales = 'free') +
  ggtitle('oxygen')

ggplot(t_2021[t_2021$variable=='temperature',], aes(x = doy, y = crps)) +
  geom_line() +
  facet_wrap(~depth, scales = 'free') +
  ggtitle('temperature')

ggplot(t_2021, aes(x = doy, y = logs)) +
  geom_line() +
  facet_grid(depth~variable, scales = 'free') 

#######################
# 2022 only
t_2022 <- tgts %>% 
  filter(year==2022) # doy > 154 & doy < 290
t_2022 <- full_join(t_2022, null)
t_2022 <- t_2022 %>% 
  filter(observed > 0,
         sd > 0) %>% 
  group_by(depth, variable, doy) %>% 
  mutate(crps = crps.numeric(y = observed, family = "normal", 
                             mean = mean, sd = sd),
         logs = logs.numeric(y = observed, family = "normal", 
                             mean = mean, sd = sd),
         rmse = Metrics::rmse(observed, mean))

ggplot(t_2022[t_2022$variable=='oxygen',], aes(x = doy, y = logs)) +
  geom_line() +
  facet_wrap(~depth, scales = 'free') +
  ggtitle('oxygen')

ggplot(t_2022[t_2022$variable=='temperature',], aes(x = doy, y = logs)) +
  geom_line() +
  facet_wrap(~depth, scales = 'free') +
  ggtitle('temperature')


scores <- full_join(t_2021, t_2022)
ggplot(scores, aes(x = doy, y = logs, color = as.factor(year))) +
  geom_line() +
  facet_grid(depth~variable)

write.csv(scores, './scores/sunp/climatology_scores.csv', row.names = FALSE)


means <- scores %>% 
  group_by(variable, depth, year) %>% 
  mutate(mean_crps = mean(crps),
         mean_log = mean(logs)) %>% 
  distinct(variable, year, depth, .keep_all = TRUE) %>% 
  dplyr::select(variable, depth, year, mean_crps, mean_log)
means
