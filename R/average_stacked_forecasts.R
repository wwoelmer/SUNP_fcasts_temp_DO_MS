# function to read in a list of stacked noaa forecasts and average them for input into flare. will look for existing file (assuming same location and name as output_directory and outfile_name) and append if data already exists


average_stacked_forecasts <- function(forecast_dates, # vector of the date range you'd like to create
                                      site,
                                      noaa_hour = 1, # numeric; whether you want to average the 1hr or 6hr forecasts
                                      noaa_directory, # file path of the directory where the stacked ensemble files are stored
                                      output_directory, # file path where you want the output file to go
                                      outfile_name = "observed-met-noaa" # prefix/name of the final output file

                                 
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
cf_units <- cf_var_units1

system_date <- lubridate::as_date(lubridate::with_tz(Sys.time(),"UTC"))

dates <- lubridate::as_date(forecast_dates)
dates <- dates[which(dates < system_date)]

identifier <- paste(outfile_name, site,sep="_")
fname <- paste0(identifier,".nc")
output_file <- file.path(output_directory, fname)

# look in output directory for existing file
hist_file <- fname
hist_files <- list.files(output_directory)
append_data <- FALSE
run_fx <- TRUE

if(hist_file %in% hist_files){
  hist_met_nc <- ncdf4::nc_open(file.path(output_file))
  hist_met_time <- ncdf4::ncvar_get(hist_met_nc, "time")
  origin <- stringr::str_sub(ncdf4::ncatt_get(hist_met_nc, "time")$units, 13, 28)
  origin <- lubridate::ymd_hm(origin)
  hist_met_time <- origin + lubridate::hours(hist_met_time)
  hist_met <- tibble::tibble(time = hist_met_time)
  
  for(i in 1:length(cf_met_vars)){
    hist_met <- cbind(hist_met, ncdf4::ncvar_get(hist_met_nc, cf_met_vars[i]))
  }
  
  names(hist_met) <- c("time", cf_met_vars) # glm_met_vars
  
  if(max(hist_met$time) == max(dates)){
    print('Already up to date, cancel the rest of the function')
    run_fx <- FALSE
  }else if(max(hist_met$time) > max(dates)){
    print('Already up to date, cancel the rest of the function')
    run_fx <- FALSE
  }else if(max(hist_met$time) > min(dates)){
    print('Appending existing historical files')
    append_data <- TRUE
    dates <- dates[dates > max(hist_met$time)]
  }else{
    append_data <- FALSE
  }
}

if(run_fx){
# read in stacked 1hr files
stacked_directory <- file.path(noaa_directory, paste0('NOAAGEFS_', noaa_hour, 'hr_stacked'), site)
stacked_files <- list.files(stacked_directory)
stacked_met_all <- NULL
#run_fx <- TRUE
#append_data <- FALSE


if(length(stacked_files) > 1){
  for(i in 1:length(stacked_files)){
    ens <- dplyr::last(unlist(stringr::str_split(basename(stacked_files[i]),"_")))
    ens <- as.numeric(stringr::str_sub(ens,4,5))
    stacked_met_nc <- ncdf4::nc_open(file.path(stacked_directory, stacked_files[i]))
    stacked_met_time <- ncdf4::ncvar_get(stacked_met_nc, "time")
    origin <- stringr::str_sub(ncdf4::ncatt_get(stacked_met_nc, "time")$units, 13, 28)
    origin <- lubridate::ymd_hm(origin)
    stacked_met_time <- origin + lubridate::hours(stacked_met_time)
    stacked_met <- tibble::tibble(time = stacked_met_time,
                               NOAA.member = ens+1)
    
    for(j in 1:length(cf_met_vars)){
      stacked_met <- cbind(stacked_met, ncdf4::ncvar_get(stacked_met_nc, cf_met_vars[j]))
    }
    
    ncdf4::nc_close(stacked_met_nc)
    
    names(stacked_met) <- c("time","NOAA.member", cf_met_vars) # glm_met_vars
    
    
    stacked_met_all <- rbind(stacked_met_all, stacked_met)
  }
  
  noaa_met_nc <- ncdf4::nc_open(file.path(stacked_directory, stacked_files[1]))
  lat <- ncdf4::ncvar_get(noaa_met_nc, "latitude")
  lon <- ncdf4::ncvar_get(noaa_met_nc, "longitude")
  ncdf4::nc_close(noaa_met_nc)

}


stacked_met_mean <- NULL
stacked_met_mean <- stacked_met_all %>% 
  dplyr::group_by(time) %>% 
  dplyr::mutate(air_temperature = mean(air_temperature, na.rm = TRUE),
                air_pressure = mean(air_pressure, na.rm = TRUE),
                relative_humidity = mean(relative_humidity, na.rm = TRUE),
                surface_downwelling_longwave_flux_in_air = mean(surface_downwelling_longwave_flux_in_air, na.rm = TRUE),
                surface_downwelling_shortwave_flux_in_air = mean(surface_downwelling_shortwave_flux_in_air, na.rm = TRUE),
                precipitation_flux = mean(precipitation_flux, na.rm = TRUE),
                specific_humidity = mean(specific_humidity, na.rm = TRUE),
                wind_speed = mean(wind_speed, na.rm = TRUE)) %>% 
  dplyr::distinct(time, .keep_all = TRUE) %>% 
  dplyr::select(-NOAA.member)  %>% 
  dplyr::arrange(time) %>% 
  dplyr::ungroup()


if(append_data==TRUE){
  stacked_met_mean <- rbind(hist_met, stacked_met_mean)
}

stacked_met_mean <- na.omit(stacked_met_mean)

# write the file
noaaGEFSpoint::write_noaa_gefs_netcdf(df = stacked_met_mean,
                                      ens = NA, 
                                      lat = lat, 
                                      lon = lon, 
                                      cf_units = cf_var_units1, 
                                      output_file = output_file, 
                                      overwrite = TRUE)



  }
}