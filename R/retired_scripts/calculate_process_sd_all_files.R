# calculate running process sd

###############################
# arguments for testing
#lake_directory <- here::here()
#folders <- c('all_UC')
#horizons <- seq(1, 35, by = 1)
#vars <- c('temperature', 'oxygen')
#depths <- c(1.0, 10.0)
#config <- FLAREr::set_configuration(configure_run_file = "configure_run.yml",
#                                    lake_directory, 
#                                    config_set_name = "UC_analysis")

###############################
calculate_process_sd <- function(lake_directory,
                                    folders, # character string of folders where forecasts are stored
                                    horizons, # which horizons you want to score across
                                    vars, # character string of variables you want to score across
                                    depths, # string of depths you have observations at
                                    config
                                    ){
  library(arrow)
  library(tidyverse)
  
  score_folder <- file.path(lake_directory, 'scores/sunp', folders[1])
  files1 <- list.files(path = score_folder, pattern = "*.parquet")
  
  # subset file to include only 30 days before the forecast start date
 # start_d <- as.Date(config$run_config$forecast_start_datetime) - days(30)
#  end_d <- as.Date(config$run_config$forecast_start_datetime)
#  date_range <- seq.Date(start_d, end_d, by = "day")
#  file_range <- paste0(config$location$site_id, "-", date_range, "-", config$run_config$sim_name, ".parquet")
  
#  files_sub <- files1[files1 %in% file_range]
  
  
  f <- arrow::read_parquet(file.path(score_folder, files1[1])) # start with second file because first is the spinup period
  
  
  # some subsetting
  f <- f %>% 
    dplyr::filter(variable %in% vars,
                  !is.na(observation)) # keep any fcasts where there are obs to compare to 

    
    # read in files
    for (i in 2:length(files1)) {
      print(i)
      temp <- arrow::read_parquet(paste0(score_folder, "/", files1[i]))
      temp <- temp %>% 
        dplyr::filter(variable %in% vars,
                      !is.na(observation))  
      f <- rbind(f, temp)
    }
    
  
  f$reference_datetime <- as.POSIXct(f$reference_datetime)
  
  # calculate sd(resid) for each depth where observations are available
  df <- f %>% 
      dplyr::filter(sd > 0) %>% 
      dplyr::filter(horizon==1) %>% 
      dplyr::distinct(depth, datetime, variable, .keep_all = TRUE)
    
    m <- df %>% 
      dplyr::select(reference_datetime, depth, datetime, variable, observation, mean, horizon) %>% 
      dplyr::group_by(datetime, variable, depth, horizon) %>% 
      dplyr::mutate(resid = observation - mean)
    
    m <- m %>% 
      dplyr::group_by(depth, variable) %>% 
      dplyr::mutate(sd_resid = sd(resid))
    
    m_out <- m %>% 
      dplyr::distinct(depth, variable, .keep_all = TRUE) %>% 
      dplyr::select(depth, variable, sd_resid) 
    m_out$sd_resid <- round(m_out$sd_resid, 2)
    
    m_wide <- m_out %>% 
      pivot_wider(names_from = 'variable', values_from = 'sd_resid') %>% 
      mutate(depth = as.numeric(depth))
    m_wide <- m_wide[order(m_wide$depth),]
      
  # assign running process error value to config files
    # for temperature, we assign all depths to depth_model_sd.csv
    depth_temp_sd <- m_wide %>% 
      dplyr::select(depth, temperature) %>% 
      dplyr::rename(temp = temperature)
    
    write.csv(depth_temp_sd, file.path(config$file_path$configuration_directory,
                                       config$model_settings$depth_model_sd_config_file),
              row.names = FALSE, quote = FALSE)
  
  # calculate sd(resid) across all depths for states_config$model_sd (1 value)
    df2 <- f %>% 
      dplyr::filter(sd > 0) %>% 
      dplyr::filter(horizon==1) %>% 
      dplyr::distinct(depth, datetime, variable, .keep_all = TRUE)
    
    m2 <- df2 %>% 
      dplyr::select(reference_datetime, depth, datetime, variable, observation, mean, horizon) %>% 
      dplyr::group_by(datetime, variable, depth, horizon) %>% 
      dplyr::mutate(resid = observation - mean)
    
    m2 <- m2 %>% 
      dplyr::group_by(variable) %>% 
      dplyr::mutate(sd_resid = sd(resid))
    
    m_out2 <- m2 %>% 
      dplyr::distinct(variable, .keep_all = TRUE) %>% 
      dplyr::select(variable, sd_resid) 
    m_out2$sd_resid <- round(m_out2$sd_resid, 2)
    
    df <- data.frame('var' = vars, 
                   state_names = c('temp', 'OXY_oxy'),
                   process_sd = NA)
  
  for (i in 1:length(vars)) {
    df$process_sd[i] <- m_out2$sd_resid[m_out2$variable==vars[i]]
  }
  
  # update process error in states_config
  st_config <- read.csv(file.path(config$file_path$configuration_directory,
                                  config$model_settings$states_config_file))
  
  for (i in 1:nrow(st_config)) {
    idx <- which(st_config$state_names[i]==df$state_names)
    if(length(idx) > 0){
      st_config$model_sd[i] <- df$process_sd[idx] 
    }
  }
  
  write.csv(st_config, file.path(config$file_path$configuration_directory,
                                 config$model_settings$states_config_file),
            row.names = FALSE, quote = FALSE)  
}

