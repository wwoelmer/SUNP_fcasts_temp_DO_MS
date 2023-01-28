# calculate running process sd

###############################
# arguments for testing
lake_directory <- here::here()
folders <- c('all_UC')
horizons <- seq(1, 35, by = 1)
vars <- c('temperature', 'oxygen')
depths <- c(1.0, 10.0)
config <- FLAREr::set_configuration(configure_run_file = "configure_run.yml",
                                    lake_directory, 
                                    config_set_name = "UC_analysis")

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
    dplyr::filter(variable %in% vars,
                  !is.na(observation)) # keep any fcasts where there are obs to compare to 

  # read in files
  starting_index <- 1
  for(j in starting_index:length(folders)){
    score_folder <- file.path(lake_directory, 'scores/sunp', folders[j])
    print(folders[j])
    
    files1 <- list.files(path = score_folder, pattern = "*.parquet")
    dataset <- arrow::read_parquet(file.path(score_folder, files1[2])) # because you read in the first file above
    
    dataset <- dataset %>% 
      dplyr::filter(variable %in% vars,
                    !is.na(observation))
    
    # read in files
    for (i in 3:length(files1)) {
      print(i)
      temp <- read_parquet(paste0(score_folder, "/", files1[i]))
      temp <- temp %>% 
        dplyr::filter(variable %in% vars,
                      !is.na(observation))  
      dataset <- rbind(dataset, temp)
    }
    
    f <- rbind(f, dataset)
    
  }
  
  out <- data.frame(horizon = NA, 
                    temperature = NA, 
                    oxygen = NA)
  for(i in 1:35){
    df <- f %>% 
      dplyr::filter(sd > 0) %>% 
      dplyr::filter(horizon==i) %>% 
      distinct(depth, datetime, variable, .keep_all = TRUE)
    
    m <- df %>% 
      dplyr::select(reference_datetime, depth, datetime, variable, observation, mean, horizon) %>% 
      group_by(datetime, variable, depth, horizon) %>% 
      mutate(resid = observation - mean)
    
    m_temp <- m %>% 
      filter(variable=='temperature') %>% 
      ungroup() %>% 
      select(-variable)
    
    m_oxy <- m %>% 
      filter(variable=='oxygen') %>% 
      ungroup() %>% 
      select(-variable, -reference_datetime, -observation, -mean) 
    
    out[i, 1] <- i
    out[i, 2] <-  sd(m_temp$resid)
    out[i, 3] <- sd(m_oxy$resid)
    

  }
  
  
  head(out)
  out <- out %>% 
    pivot_longer(temperature:oxygen, names_to = 'variable', values_to = 'sd_residual')
  
  ggplot(out, aes(x = horizon, y = sd_residual)) +
    facet_wrap(~variable, scales = 'free_y') +
    geom_point()

  ggplot(m_temp, aes(x = resid, fill = depth)) +
    geom_histogram()
  
  ggplot(m_oxy, aes(x = resid, fill = depth)) +
    geom_histogram()
  
  
  # calculate running process error value
  df <- data.frame('var' = vars, 
                   state_names = c('temp', 'OXY_oxy'),
                   process_sd = NA)
  
  for (i in 1:length(vars)) {
    df$process_sd[i] <- out$sd_residual[out$variable==vars[i] & out$horizon==1]
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

