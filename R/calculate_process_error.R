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
calculate_process_error <- function(lake_directory,
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
  f <- arrow::read_parquet(file.path(score_folder, files1[2])) # start with second file because first is the spinup period
  
  # some subsetting
  f <- f %>% 
    tidyr::separate(site_id, into = c("site", "depth"), sep = "-") %>% 
    dplyr::filter(variable %in% vars,
           depth %in% depths)  

  # read in files
  starting_index <- 1
  for(j in starting_index:length(folders)){
    score_folder <- file.path(lake_directory, 'scores/sunp', folders[j])
    print(folders[j])
    
    files1 <- list.files(path = score_folder, pattern = "*.parquet")
    dataset <- arrow::read_parquet(file.path(score_folder, files1[2]))
    
    dataset <- dataset %>% 
      tidyr::separate(site_id, into = c("site", "depth"), sep = "-") %>% 
      dplyr::filter(variable %in% vars,
             depth %in% depths)  
    
    # read in files
    for (i in 3:length(files1)) {
      print(i)
      temp <- read_parquet(paste0(score_folder, "/", files1[i]))
      temp <- temp %>% 
        tidyr::separate(site_id, into = c("site", "depth"), sep = "-") %>% 
        dplyr::filter(variable %in% vars,
               depth %in% depths)  
      dataset <- rbind(dataset, temp)
    }
    
    f <- rbind(f, dataset)
    
  }
  
  
  
  f <- f %>% 
    dplyr::filter(sd > 0) %>% 
    dplyr::filter(horizon > 0) %>% 
    dplyr::filter(horizon %in% horizons) %>% 
    dplyr::mutate(obs_mgL_C = ifelse(variable=='oxygen', observation*32/1000, observation),
           mean_mgL_C = ifelse(variable=='oxygen', mean*32/1000, mean),
           crps2 = scoringRules::crps(observation, family = "normal", mean = mean, sd = sd),
           crps2_mgL = ifelse(variable=='oxygen', crps2*32/1000, crps2))

  #write.csv(f, file.path(lake_directory, 'all_obs_fcasts.csv'), row.names = FALSE)
  
  # calculate running process error value
  df <- data.frame('var' = vars, 
                   state_names = c('temp', 'OXY_oxy'),
                   process_sd = NA)
  
  for (i in 1:length(vars)) {
    df$process_sd[i] <- mean(f$crps2[f$variable==vars[i]])
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
            row.names = FALSE)  
}

