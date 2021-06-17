# combine first cycle noaa forecasts as obs


stack_noaa_forecasts <- function(dates, # list of dates you have NOAA GEFS .nc forecasts for 
                                 cycle = '00',
                                 outfile # file path where you want the output file to go
                                 ){
  
  cf_met_vars <- c("air_temperature",
                   "surface_downwelling_shortwave_flux_in_air",
                   "surface_downwelling_longwave_flux_in_air",
                   "relative_humidity",
                   "wind_speed",
                   "precipitation_flux")
  
  glm_met_vars <- c("AirTemp",
                    "ShortWave",
                    "LongWave",
                    "RelHum",
                    "WindSpeed",
                    "Rain")
  
  noaa_obs_out <- data.frame(matrix(ncol=length(glm_met_vars)), nrow = 0)
  colnames(noaa_obs_out) <- c('time', glm_met_vars)
  noaa_obs_out$time <- as.POSIXct(noaa_obs_out$time)
  
  
  for(k in 1:length(dates)){
    
    forecast_dir <- file.path(config$file_path$noaa_directory, 'drivers', 'noaa-point', 'NOAAGEFS_1hr', 'sunp', dates[k], cycle)
    
    if(!is.null(forecast_dir)){
      
      forecast_files <- list.files(forecast_dir, pattern = ".nc", full.names = TRUE)
      
      #forecast_files <- forecast_files[!stringr::str_detect(string = forecast_files, pattern = basename(obs_met_file))]
      
      nfiles <-   length(forecast_files)
      
    }else if(!is.null(met)){
      
      nfiles <-   1
    }
    
    if(length(forecast_files) < 1){
      write.csv(noaa_obs_out, paste0(outfile, '/NOAA_GEFS_mean_ens_', dates[1], '-', dates[k-1], '.csv'), row.names = FALSE)
    }
      daily_noaa <- data.frame(matrix(ncol = length(glm_met_vars) + 2, nrow = 0))
      colnames(daily_noaa) <- c('time', glm_met_vars, 'ens')
      
      for(j in 1:nfiles){
        
        if(!is.null(forecast_dir) & config$met$use_forecasted_met){
          
          
          ens <- dplyr::last(unlist(stringr::str_split(basename(forecast_files[j]),"_")))
          ens <- stringr::str_sub(ens,1,5)
          noaa_met_nc <- ncdf4::nc_open(forecast_files[j])
          noaa_met_time <- ncdf4::ncvar_get(noaa_met_nc, "time")
          origin <- stringr::str_sub(ncdf4::ncatt_get(noaa_met_nc, "time")$units, 13, 28)
          origin <- lubridate::ymd_hm(origin)
          noaa_met_time <- origin + lubridate::hours(noaa_met_time)
          noaa_met <- tibble::tibble(time = noaa_met_time)
          
          for(i in 1:length(cf_met_vars)){
            noaa_met <- cbind(noaa_met, ncdf4::ncvar_get(noaa_met_nc, cf_met_vars[i]))
          }
          
          ncdf4::nc_close(noaa_met_nc)
          names(noaa_met) <- c("time", glm_met_vars)
          
          noaa_met <- noaa_met %>%
            dplyr::mutate(ens = j) %>% 
            dplyr::mutate(date = lubridate::as_date(time)) %>% 
            dplyr::filter(date %in% dates[k]) %>% 
            select(-date)
          
          daily_noaa <- rbind(noaa_met, daily_noaa)
        }
      }
      
      daily_noaa_mean <- daily_noaa %>% 
        dplyr::group_by(time) %>% 
        dplyr::mutate(AirTemp = mean(AirTemp),
                      ShortWave = mean(ShortWave),
                      LongWave = mean(LongWave),
                      RelHum = mean(RelHum),
                      WindSpeed = mean(WindSpeed),
                      Rain = mean(Rain)) %>% 
        distinct(time, .keep_all = TRUE) %>% 
        select(-ens)
      
      noaa_obs_out <- rbind(noaa_obs_out, daily_noaa_mean)
      
    }
      
    }


