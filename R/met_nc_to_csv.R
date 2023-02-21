

met_nc_to_csv <- function(input_met_nc,
                          config,
                          output_dir){

  dat <- ncdf4::nc_open(input_met_nc)
  cf_met_vars <- c("air_temperature",
                   "surface_downwelling_shortwave_flux_in_air",
                   "surface_downwelling_longwave_flux_in_air",
                   "relative_humidity",
                   "wind_speed",
                   "precipitation_flux")
  
  obs_met_time <- ncdf4::ncvar_get(dat, "time")
  origin <- stringr::str_sub(ncdf4::ncatt_get(dat, "time")$units, 13, 28)
  origin <- lubridate::ymd_hm(origin)
  obs_met_time <- origin + lubridate::hours(obs_met_time)
  met <- tibble::tibble(time = obs_met_time)
  met <- tibble::tibble(time = obs_met_time)
  
  for(i in 1:length(cf_met_vars)){
    
    met <- cbind(met, ncdf4::ncvar_get(dat, cf_met_vars[i]))
  }
  
  ncdf4::nc_close(dat)
  names(met) <- c("time", cf_met_vars)
  met <- met %>% 
    distinct(time, .keep_all = TRUE)
  met2 <- met %>% 
    pivot_longer(air_temperature:precipitation_flux, names_to = 'variable', values_to = 'observation') %>% 
    rename(datetime = time)
  
  if(!file.exists(output_dir)){
    dir.create(output_dir)
  }
  write.csv(met2, file.path(output_dir, paste0("observed-met_",config$location$site_id,".csv")),
            row.names = FALSE, quote = FALSE)
  
  
}


