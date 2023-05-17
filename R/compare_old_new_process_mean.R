# compare forecast to observed
# compare UC bounds from each UC source to observations:
#   are we falling within the bounds for all sources?

library(arrow)
library(tidyverse)
lake_directory <- here::here()

folder_old <- file.path(lake_directory, 'scores/sunp_process_sd_old/all_UC')
files_old <- list.files(path = folder_old, pattern = "*.parquet")
f_old <- read_parquet(file.path(folder_old, files_old[1])) # start with second file because first is the spinup period

# some subsetting
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)
#horizons <- c(1, 7, 20, 35)

f_old <- f_old %>% 
  filter(variable %in% vars,
         depth %in% depths)  

  # read in files
  for (i in 2:length(files_old)) {
    print(i)
    temp <- read_parquet(paste0(folder_old, "/", files_old[i]))
    temp <- temp %>% 
      filter(variable %in% vars,
             depth %in% depths)  
    f_old <- rbind(f_old, temp)
  }
  
########################################################### 
# now for the new files
folder_new <- file.path(lake_directory, 'scores/sunp/all_UC')
files_new <- list.files(path = folder_new, pattern = "*.parquet")
f_new <- read_parquet(file.path(folder_new, files_new[2])) # start with second file because first is the spinup period

f_new <- f_new %>% 
  filter(variable %in% vars,
         depth %in% depths) 
# read in files
for (i in 3:length(files_new)) {
  print(i)
  temp <- read_parquet(paste0(folder_new, "/", files_new[i]))
  temp <- temp %>% 
    filter(variable %in% vars,
           depth %in% depths)  
  f_new <- rbind(f_new, temp)
}

f_new <- f_new %>% 
  filter(f_new$reference_datetime %in% f_old$reference_datetime) %>% 
  select(reference_datetime, datetime, variable, depth, horizon, observation, mean) %>% 
  rename(mean_new = mean)

f_old <- f_old %>% 
  filter(f_old$reference_datetime %in% f_new$reference_datetime) %>% 
  select(reference_datetime, datetime, variable, depth, horizon, observation, mean) %>% 
  rename(mean_old = mean)


f_all <- full_join(f_new, f_old) %>%
  filter(horizon > 0) %>% 
  mutate(diff = mean_old - mean_new) 

ggplot(f_all[f_all$horizon==7,], aes(x = datetime, y = mean_old, color = 'old')) +
  geom_line() +
  geom_line(aes(x = datetime, y = mean_new, color = 'new')) +
  geom_point(aes(x = datetime, y= observation, color = 'obs')) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') +
  ggtitle('horizon = 7')

ggplot(f_all[f_all$horizon==3,], aes(x = datetime, y = mean_old, color = 'old')) +
  geom_line() +
  geom_line(aes(x = datetime, y = mean_new, color = 'new')) +
  geom_point(aes(x = datetime, y= observation, color = 'obs')) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') +
  ggtitle('horizon = 3')

ggplot(f_all[f_all$horizon==14,], aes(x = datetime, y = mean_old, color = 'old')) +
  geom_line() +
  geom_line(aes(x = datetime, y = mean_new, color = 'new')) +
  geom_point(aes(x = datetime, y= observation, color = 'obs')) +
  facet_grid(cols = vars(depth), rows = vars(variable), scale = 'free') +
  ggtitle('horizon = 14')
