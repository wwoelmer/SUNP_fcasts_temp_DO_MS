# combine first cycle noaa forecasts as obs


stack_noaa_forecasts <- function(dates, # list of dates you have NOAA GEFS .nc forecasts for 
                                 site,
                                 noaa_directory, # directory where you have noaa forecasts stored
                                 noaa_model, #name of hte noaa model, e.g. "noaa/NOAAGEFS_6hr"
                                 outfile, # file path where you want the output file to go
                                 model_name = "observed-met-noaa" # file name of the output
){
  
  cf_met_vars <- c("air_temperature", 
                   "air_pressure", 
                   "relative_humidity", 
                   "surface_downwelling_longwave_flux_in_air",
                   "surface_downwelling_shortwave_flux_in_air", 
                   "precipitation_flux",
                   "specific_humidity",
                   "wind_speed")
  
  cf_var_units1 <- c("K", 
                     "Pa", 
                     "1", 
                     "Wm-2", 
                     "Wm-2", 
                     "kgm-2s-1", 
                     "1", 
                     "ms-1")  #Negative numbers indicate negative exponents
  
  model_name <- model_name
  cf_units <- cf_var_units1
  identifier <- paste(model_name, site,sep="_")
  fname <- paste0(identifier,".nc")
  output_file <- file.path(outfile, fname)
  stacked_directory <- file.path(outfile, paste0(noaa_model, "_stacked"))
  
  # set up directories
  if(!dir.exists(stacked_directory)){
    dir.create(stacked_directory, recursive=TRUE, showWarnings = FALSE)
  }

   
  # check output directory to see if there are already existing files to append to
  hist_files <- list.files(file.path(stacked_directory, site))
  hist_met_all <- NULL
  run_fx <- TRUE
  append_data <- FALSE
  

  if(length(hist_files) > 1){
    for(i in 1:length(hist_files)){
      hist_met_nc <- ncdf4::nc_open(file.path(stacked_directory, site, hist_files[i]))
      hist_met_time <- ncdf4::ncvar_get(hist_met_nc, "time")
      origin <- stringr::str_sub(ncdf4::ncatt_get(hist_met_nc, "time")$units, 13, 28)
      origin <- lubridate::ymd_hm(origin)
      hist_met_time <- origin + lubridate::hours(hist_met_time)
      hist_met <- tibble::tibble(time = hist_met_time)
      
      for(j in 1:length(cf_met_vars)){
        hist_met <- cbind(hist_met, ncdf4::ncvar_get(hist_met_nc, cf_met_vars[j]))
      }
      
      names(hist_met) <- c("time", cf_met_vars) # glm_met_vars
      hist_met$ens <- i-1
      
      hist_met_all <- rbind(hist_met_all, hist_met)
    }
    
    if(max(hist_met_all$time) == max(dates)){
      print('Already up to date, cancel the rest of the function')
      run_fx <- FALSE
    }else if(max(hist_met_all$time) > max(dates)){
      print('Already up to date, cancel the rest of the function')
      run_fx <- FALSE
    }else if(max(hist_met_all$time) > min(dates)){
      print('Appending existing historical files')
      append_data <- TRUE
      dates <- dates[dates > max(hist_met_all$time)]
    }else{
      append_data <- FALSE
    }
  }
  


  # loop through each date of forecasts and extract the first day, stack together to create a continuous dataset of day 1 forecasts
  
  # set up dataframe for outfile
  noaa_obs_out <- NULL
  if(run_fx){
  for(k in 1:length(dates)){
    
    noaa_model_directory <- file.path(noaa_directory, noaa_model, site, dates[k])
    
    cycle <- list.files(noaa_model_directory)
    
    daily_noaa <- NULL

    for(f in 1:length(cycle)){
      forecast_dir <- file.path(noaa_model_directory, cycle[f])
      print(forecast_dir)
      
      if(!is.null(forecast_dir)){
        
        forecast_files <- list.files(forecast_dir, pattern = ".nc", full.names = TRUE)
        nfiles <- length(forecast_files)
        
      }
      
      for(j in 1:nfiles){
        
        if(!is.null(forecast_dir)) {
          
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
          names(noaa_met) <- c("time", cf_met_vars) # glm_met_vars
          
          noaa_met <- noaa_met %>%
            dplyr::mutate(ens = j) %>% 
            dplyr::mutate(date = lubridate::as_date(time)) %>% 
            dplyr::filter(date %in% dates[k]) %>% # select just the first day
            #dplyr::mutate(cycle = cycle[f]) %>% 
            select(-date)
            
          #if(length(cycle > 1)){
          #   fluxes <- noaa_met[2, c(5,6,7)]
          #   noaa_met <- noaa_met[1,]
          #   noaa_met[1 , c(5,6,7)] <-  fluxes
          #}
          if(cycle > 1){ #if you have more than just the 00 cycle and want the first six hours of each
            noaa_met <- noaa_met[c(1:6),] # FIX THIS FOR 1hr vs 6hr
          }
          daily_noaa <- dplyr::bind_rows(noaa_met, daily_noaa)
        }
      }
    }
    
    noaa_met_nc <- ncdf4::nc_open(forecast_files[1])
    lat <- ncdf4::ncvar_get(noaa_met_nc, "latitude")
    lon <- ncdf4::ncvar_get(noaa_met_nc, "longitude")
    
    noaa_obs_out <- rbind(noaa_obs_out, daily_noaa)
   
    
    
  }
  
  if(append_data==TRUE){
    noaa_obs_out <- rbind(hist_met_all, noaa_obs_out)
  }
  
  #noaa_obs_out <- na.omit(noaa_obs_out)
  forecast_noaa <- noaa_obs_out %>% 
    rename(NOAA.member = ens) %>% 
    select(time, NOAA.member, air_temperature, 
           air_pressure, relative_humidity, surface_downwelling_longwave_flux_in_air, 
           surface_downwelling_shortwave_flux_in_air, precipitation_flux, 
           specific_humidity, wind_speed) %>% 
    arrange(time, NOAA.member)
  
  for (ens in 1:31) { # i is the ensemble number
    
    #Turn the ensemble number into a string
    if((ens-1)< 10){
      ens_name <- paste0("0",ens-1)
    }else{
      ens_name <- ens-1
    }
    
    forecast_noaa_ens <- forecast_noaa %>%
      dplyr::filter(NOAA.member == ens)
    
    forecast_noaa_ens$surface_downwelling_longwave_flux_in_air[2:nrow(forecast_noaa_ens)] <- forecast_noaa_ens$surface_downwelling_longwave_flux_in_air[1:(nrow(forecast_noaa_ens)-1)]
    
    forecast_noaa_ens$surface_downwelling_shortwave_flux_in_air[2:nrow(forecast_noaa_ens)] <- forecast_noaa_ens$surface_downwelling_shortwave_flux_in_air[1:(nrow(forecast_noaa_ens)-1)]
    
    forecast_noaa_ens$precipitation_flux[2:nrow(forecast_noaa_ens)] <- forecast_noaa_ens$precipitation_flux[1:(nrow(forecast_noaa_ens)-1)]
    
    #forecast_noaa_ens$surface_downwelling_longwave_flux_in_air[1] <- NA
    #forecast_noaa_ens$surface_downwelling_shortwave_flux_in_air[1] <- NA
    #forecast_noaa_ens$precipitation_flux[1] <- NA
    
    end_date <- forecast_noaa_ens %>%
      dplyr::summarise(max_time = max(time))
    
    model_name_6hr <- paste0(model_name, "-6hr")
    identifier <- paste(model_name_6hr, site, format(dplyr::first(forecast_noaa_ens$time), "%Y-%m-%dT%H"), sep="_")
    model_site_dir <- file.path(stacked_directory, site)
    
    if(!dir.exists(model_site_dir)){
      dir.create(model_site_dir, recursive=TRUE, showWarnings = FALSE)
    }
    
    fname <- paste0(identifier,"_ens",ens_name,".nc")
    output_file <- file.path(model_site_dir,fname)
    
    #Write netCDF
    noaaGEFSpoint::write_noaa_gefs_netcdf(df = forecast_noaa_ens,ens, lat = lat, lon = lon, cf_units = cf_var_units1, output_file = output_file, overwrite = TRUE)
    
    stacked_directory_1hr <- file.path(outfile, 'noaa', "NOAAGEFS_1hr_stacked")
    model_site_dir_1hr <- file.path(stacked_directory_1hr, site)
    
    if(!dir.exists(model_site_dir_1hr)){
      dir.create(model_site_dir_1hr, recursive=TRUE, showWarnings = FALSE)
    }
    
    model_name_1hr <- paste0(model_name, "-1hr")
    identifier_ds <- paste(model_name_1hr, site, format(dplyr::first(forecast_noaa_ens$time), "%Y-%m-%dT%H"), sep="_")
    fname_ds <- file.path(model_site_dir_1hr, paste0(identifier_ds,"_ens",ens_name,".nc"))
    
    #Run downscaling
    noaaGEFSpoint::temporal_downscale(input_file = output_file, output_file = fname_ds, overwrite = TRUE, hr = 1)
  }
}

}




