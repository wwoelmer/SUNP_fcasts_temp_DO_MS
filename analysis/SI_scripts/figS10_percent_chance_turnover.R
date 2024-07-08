# read in forecasts 40 days before turnover in each year, 
# calculate schmidt stability leading up to turnover
# OR calculate temp difference between surface and bottom (<1C is turnover)
# how many days ahead of turnover did forecasts predict <1C

library(tidyverse)
library(ggpubr)

lake_directory <- here::here()
folder <- 'SUNP_fcasts_temp_DO'
mix_dates <- c('2021-10-04', '2022-09-23')
start_dates <- as.Date(mix_dates) - days(40)

sub_dates <- c(seq.Date(start_dates[1], as.Date(mix_dates[1]) + 7, by = 'day'),
                    seq.Date(start_dates[2], as.Date(mix_dates[2]) + 7, by = 'day'))
sub_dates

forecast_folder <- file.path(lake_directory, 'forecasts/sunp/', folder)
filenames <- list.files(path = forecast_folder, pattern = "*.csv.gz")

# Extract dates from filenames using a regular expression
extract_dates <- function(filenames) {
  # Regular expression to match dates in YYYY-MM-DD format
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  
  # Extract dates from filenames
  date_strings <- regmatches(filenames, regexpr(date_pattern, filenames))
  
  # Convert the extracted date strings to Date objects
  extracted_dates <- as.Date(date_strings)
  
  return(extracted_dates)
}

extracted_dates <- extract_dates(filenames)
sub_files <- filenames[extracted_dates %in% sub_dates]

###########################################################
# some subsetting
vars <- c('temperature')
depths <- c(0.1, 10)
horizons <- c(1:35)


## read in teh first files
files1 <- read.csv(paste0(forecast_folder, "/", sub_files[1]))

# reaad in the first file
files1 <- files1 %>% 
  filter(variable %in% vars,
         forecast > 0,
         depth %in% depths) %>% 
  mutate(horizon = difftime(as.POSIXct(datetime), as.POSIXct(reference_datetime), units = 'days')) %>% 
  pivot_wider(names_from = 'depth', values_from = 'prediction', names_glue = "depth_{depth}") %>% 
  group_by(horizon, reference_datetime) %>% 
  mutate(temp_diff = depth_0.1 - depth_10)


files_summ <- files1 %>% 
  group_by(horizon, reference_datetime, datetime) %>% 
  summarise(total = n(),
            above_1 = sum(temp_diff < 1),
            percent_turnover = (above_1/total)*100) %>% 
  ungroup() 



# loop through the rest
for(i in 2:length(sub_files)){
  print(sub_files[i])
  temp <- read.csv(paste0(forecast_folder, "/", sub_files[i]))
  
  temp <- temp %>% 
    filter(variable %in% vars,
           forecast > 0,
           depth %in% depths) %>% 
    mutate(horizon = difftime(as.POSIXct(datetime), as.POSIXct(reference_datetime), units = 'days')) %>% 
    pivot_wider(names_from = 'depth', values_from = 'prediction', names_glue = "depth_{depth}") %>% 
    group_by(horizon, reference_datetime) %>% 
    mutate(temp_diff = depth_0.1 - depth_10)
  
  if(temp$horizon[1] <1){
    temp$horizon <- round(temp$horizon)
  }

  
  temp_summ <- temp %>% 
    group_by(horizon, reference_datetime, datetime) %>% 
    summarise(total = n(),
              above_1 = sum(temp_diff < 1),
              percent_turnover = (above_1/total)*100) %>% 
    ungroup() 
  
  files_summ <- rbind(files_summ, temp_summ)
}

files_summ <- files_summ %>% 
  mutate(year = year(reference_datetime)) %>% 
  mutate(mix_date = ifelse(year==2021, '2021-10-04', '2022-09-23'))


library(plotly)
ggplot(files_summ, aes(x = as.Date(datetime), y = percent_turnover, color = reference_datetime)) +
  geom_line() +
  geom_vline(aes(xintercept = as.Date(mix_date))) +
  geom_hline(aes(yintercept = 1)) +
  facet_wrap(~year, scales = 'free') +
  theme_bw() +
  theme(legend.position = "none") 


turn <- files_summ %>% 
  filter(as.Date(datetime) %in% unique(mix_dates)) %>% 
  mutate(horizon = round(horizon),
         days_before_turnover = as.Date(mix_date) - as.Date(datetime))

ggplot(turn, aes(x = horizon, y = percent_turnover)) +
  geom_point() +
  scale_x_reverse() +
  facet_wrap(~year, scales = 'free')



chance_turnover <- ggplot(turn, aes(x = horizon, y = percent_turnover)) +
  geom_point() +
  geom_line() +
  scale_x_reverse() +
  geom_hline(aes(yintercept = 50)) +
  facet_wrap(~year) +
  xlab('Days before turnover') +
  ylab('Percent chance of turnover') +
  theme_bw()

chance_turnover
ggsave('./figures/figs10_chance_turnover.tiff', chance_turnover, scale = 0.6, dpi = 300, 
       unit = "mm", width = 400, height = 250)


obs <- read.csv('./targets/sunp/SUNP_fcasts_temp_DO/sunp-targets-insitu.csv') %>% 
  filter(as.Date(time) > as.Date('2021-06-08'),
         variable=='temperature') %>% 
  mutate(year = year(time),
         date = as.Date(time))


obs <- obs %>% 
  filter(year!='2023') %>% 
  group_by(date) %>% 
  arrange(depth) %>% 
  filter(depth == 10| depth ==0.1) %>% 
  pivot_wider(names_from = 'depth', values_from = 'observed', names_glue = "depth_{depth}") %>% 
  mutate(temp_diff = depth_0.1 - depth_10)

ggplot(obs, aes(x = as.Date(date), y = temp_diff)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept = 1)) +
  facet_wrap(~year, scales = 'free')

