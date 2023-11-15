


realtimefile_header<-read.csv(file.path(config$file_path$data_directory, config_obs$met_raw_obs_fname[1]), skip=1, as.is=T) #get header minus wonky Campbell rows
realtimefile_data<-read.csv(file.path(config$file_path$data_directory, config_obs$met_raw_obs_fname[1]), skip=4, header=F) #get data minus wonky Campbell rows
names(realtimefile_data)<-names(realtimefile_header) #combine the names to deal with Campbell logger formatting

# convert to UTC
realtimefile_data$TIMESTAMP <- as.POSIXct(realtimefile_data$TIMESTAMP)
attr(realtimefile_data$TIMESTAMP, "tzone") <- "UTC"

# remove days before buoy was deployed
d <- realtimefile_data
d <- d %>% filter(TIMESTAMP > as.POSIXct('2021-06-07 12:00:00'))

# combine with historical data? what to do about met at sunapee????? some conversion over to NLDAS?


# qaqc based on upper and lower bounds for variables


# rename columns




# format file name and 
model_name <- "observed-met"
site <- "sunp"
lat <- 43.3913
lon <- 360-72.0576
start_time <- dplyr::first((d$time))
end_time <- dplyr::last((d$time))
cf_units <- cf_var_units1

identifier <- paste(model_name, site,sep="_")

fname <- paste0(identifier,".nc")

output_file <- file.path(cleaned_met_file_dir, fname)

start_time <- min(d$time)
end_time <- max(d$time)

data <- d_full %>%
  dplyr::select(-time)

diff_time <- as.numeric(difftime(d_full$time, d_full$time[1], units = "hours"))

cf_var_names <- names(data)

time_dim <- ncdf4::ncdim_def(name="time",
                             units = paste("hours since", format(start_time, "%Y-%m-%d %H:%M")),
                             diff_time, #GEFS forecast starts 6 hours from start time
                             create_dimvar = TRUE)
lat_dim <- ncdf4::ncdim_def("latitude", "degree_north", lat, create_dimvar = TRUE)
lon_dim <- ncdf4::ncdim_def("longitude", "degree_east", lon, create_dimvar = TRUE)

dimensions_list <- list(time_dim, lat_dim, lon_dim)

nc_var_list <- list()
for (i in 1:length(cf_var_names)) { #Each ensemble member will have data on each variable stored in their respective file.
  nc_var_list[[i]] <- ncdf4::ncvar_def(cf_var_names[i], cf_units[i], dimensions_list, missval=NaN)
}

nc_flptr <- ncdf4::nc_create(output_file, nc_var_list, verbose = FALSE, )

#For each variable associated with that ensemble
for (j in 1:ncol(data)) {
  # "j" is the variable number.  "i" is the ensemble number. Remember that each row represents an ensemble
  ncdf4::ncvar_put(nc_flptr, nc_var_list[[j]], unlist(data[,j]))
}

ncdf4::nc_close(nc_flptr)  #Write to the disk/storage