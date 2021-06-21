realtime_file <- paste0(config$file_path$data_directory, "/", config_obs$insitu_obs_fname[1])
qaqc_file,
maintenance_url <- 'https://docs.google.com/spreadsheets/d/1IfVUlxOjG85S55vhmrorzF5FQfpmCN2MROA_ttEEiws/edit?usp=sharing'
input_file_tz,
focal_depths,
local_tzone,
config

temp_qaqc <- function(realtime_file,
                      surface_sonde,
                      profiles,
                      qaqc_file,
                      maintenance_file,
                      input_file_tz,
                      focal_depths,
                      local_tzone,
                      config){
  
  library(gsheet)
  
  d_head<-read.csv(realtime_file, skip=1, as.is=T) #get header minus wonky Campbell rows
  d <-read.csv(realtime_file, skip=4, header=F) #get data minus wonky Campbell rows
  names(d)<-names(d_head) #combine the names to deal with Campbell logger formatting
  
  #Removes row if the TIMESTAMP column is blank
  d <- d[complete.cases(d$TIMESTAMP),]
  
  #Removes row if the RECORD column has an NA or blank
  d <- d[!(is.na(d$RECORD) | d$RECORD==""), ]
  
  
  # convert to UTC
  d$TIMESTAMP <- as.POSIXct(d$TIMESTAMP)
  attr(d$TIMESTAMP, "tzone") <- "UTC"
  
  # remove data from before buoy was deployed
  d <- d[d$TIMESTAMP > '2021-06-07 00:00:00',]
  
  # remove days where maintenance occurred
  maint <- gsheet2tbl(maintenance_url)
  maint$colnumber <- NA
  
  for(i in 1:nrow(maint)){
    
    # get start and end time of one maintenance event
    start <- maint$TIMESTAMP_start[i]
    end <- maint$TIMESTAMP_end[i]
    
    # set colnumbers for maintenance events
    if(maint$instrument[i]=='ALL'){
      maint$colnumber[i] <- 'c(6:38)'
    }
    if(maint$instrument[i]=='EXO'){
      maint$colnumber[i] <- 'c(17:35)'
    }  
    if(maint$instrument[i]=='temp'){
      maint$colnumber[i] <- 'c(6:16)'
    }
    if(maint$instrument[i]=='do'){
      maint$colnumber[i] <- 'c(36:38)'
    }
    
    if(grepl("^\\d+$", maint$colnumber[i])) # single num
    {
      maintenance_cols <- intersect(c(2:38), as.integer(maint$colnumber[i]))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", maint$colnumber[i])) # c(x;y;...)
    {
      maintenance_cols <- intersect(c(2:38), as.integer(unlist(regmatches(maint$colnumber[i],
                                                                          gregexpr("\\d+", maint$colnumber[i])))))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", maint$colnumber[i])) # c(x:y)
    {
      bounds <- as.integer(unlist(regmatches(maint$colnumber[i], gregexpr("\\d+", maint$colnumber[i]))))
      maintenance_cols <- intersect(c(2:38), c(bounds[1]:bounds[2]))
    }
    
    else
    {
      warning(paste("Could not parse column colnumber in row", i, "of the maintenance log. Skipping maintenance for",
                    "that row. The value of colnumber should be in one of three formats: a single number (\"47\"), a",
                    "semicolon-separated list of numbers in c() (\"c(47;48;49)\"), or a range of numbers in c() (\"c(47:74)\").",
                    "Other values (even valid calls to c()) will not be parsed properly."))
      next
    }
    
    # remove EXO_Date and EXO_Time columns from the list of maintenance columns, because they will be deleted later
    maintenance_cols <- setdiff(maintenance_cols, c(17, 18))
    
    if(length(maintenance_cols) == 0)
    {
      warning(paste("Did not parse any valid data columns in row", i, "of the maintenance log. Valid columns have",
                    "indices 2 through 38, excluding 17 and 18, which are EXO Date and time columns and are deleted by this script. 
                    Skipping maintenance for that row."))
      next
    }
    # replace relevant data with NAs and set "all" flag while maintenance was in effect
    d[d$TIMESTAMP >= start & d$TIMESTAMP <= end, maintenance_cols] <- NA
    #d[d$TIMESTAMP >= start & d$TIMESTAMP <= end, "Flag_All"] <- 1
}
  
  
  
  
  d_therm <- d %>% dplyr::rename(timestamp = timestamp,
                                 depth = depth,
                                 value = value) %>%
    dplyr::mutate(variable = "temperature",
                  method = "thermistor",
                  value = ifelse(is.nan(value), NA, value),
                  hour = lubridate::hour(timestamp))%>%
    rename(date = timestamp)%>%
    filter(date < "2020-12-05 00:00:00")
  
  d_top <- readr::read_csv(surface_sonde)
  
  d_therm_top <- d_top %>% select(-X1)%>%
    dplyr::rename(timestamp = dateTime,
                  depth = sensorDepth,
                  value = waterTemp) %>%    
    dplyr::mutate(timestamp = mdy_hm(timestamp))%>%
    rename(date = timestamp)%>%
    arrange(date)%>%
    na.omit(.)%>%
    mutate(time = lubridate::floor_date(date, unit = "hour"))%>%
    group_by(time) %>%
    summarize_at(c("value"), mean, na.rm = TRUE)%>%
    mutate(variable = "temperature",
           method = "sonde",
           value = ifelse(is.nan(value), NA, value),
           value = ifelse(value ==0,NA,value),
           value = ifelse(value ==25, NA, value),
           hour = lubridate::hour(time),
           depth = 0.5)%>%
    select(time, hour, depth, value, variable, method)%>%
    rename(date = time)
  
  
  d_prof <- readr::read_csv(profiles)%>%
    rename(date = timestamp)
  
  d <- d_therm %>% mutate(depth = as.numeric(depth))
  d_top <- d_therm_top %>% mutate(depth = as.numeric(depth))
  d_prof <- d_prof %>% mutate(depth = as.numeric(depth))
  
  d <- bind_rows(d,d_top)
  d <- bind_rows(d, d_prof)
  
  write_csv(d, paste0(config$qaqc_data_location,"/observations_postQAQC_long.csv"))
}