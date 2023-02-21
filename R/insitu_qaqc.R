
insitu_qaqc <- function(realtime_file,
                        hist_buoy_file,
                        hist_manual_file,
                        hist_all_file,
                        maintenance_url,
                        variables,
                        cleaned_insitu_file,
                        lake_directory,
                        config){
  
  # create combined manual and high frequency buoy data file
  library(tidyverse)
  library(lubridate)
  
  sim_folder <- lake_directory
  
  # download manual data from zenodo: https://zenodo.org/record/4652076#.YKKBbqhKg2x
  manual <- read.csv(hist_manual_file)
  manual <- manual %>% 
    dplyr::filter(parameter == 'temp_C') %>% 
    dplyr::mutate(date = as.Date(date)) %>% 
    dplyr::select(date, depth_m, parameter, value, station) %>% 
    tidyr::pivot_wider(names_from = parameter, values_from = value) %>% 
    tidyr::unchop(everything()) # do this bc of strange formating with pivot wider
  
  manual <- manual %>% 
    dplyr::filter(station == 210) %>%  # this is the deep hole site
    dplyr::mutate(time = hms("12:00:00")) %>% 
    dplyr::mutate(DateTime = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC+5') + 60*60*16) %>% 
    dplyr::select(DateTime, depth_m, temp_C, station) %>% 
    dplyr::mutate(method = 'manual')
  colnames(manual) <- c('DateTime', 'Depth', 'Temp_manual', 'site', 'method')  
  manual$site <- as.character(manual$site)
  manual$DateDepth <- paste0(manual$DateTime, " ", manual$Depth)
  
  
  # and historical high frequency buoy data
  # extract noon measurements only and only observations when buoy is deployed
  field_all <- read.csv(hist_buoy_file[1])
  field_all$datetime <- as.POSIXct(field_all$datetime, format = "%Y-%m-%d %H:%M:%S")
  field_noon <- field_all %>% 
    dplyr::mutate(day = day(datetime)) %>% 
    dplyr::mutate(hour = hour(datetime)) %>% 
    dplyr::mutate(minute = minute(datetime))
  field_noon <- field_noon[field_noon$hour=='12' & field_noon$minute=='0',]
  field_noon <- field_noon[field_noon$location=='loon',]  
  field_noon <- field_noon %>% dplyr::select(-location, -day, -minute, -hour)
  
  # add depth column and remove from column name
  field_format <- data.frame("DateTime" = as.Date(NA),
                             "Depth" = NA,
                             "Temp" = NA
  )
  
  depths <- c('0.5', '0.75', '0.85', '1.0', '1.5', '1.75', '1.85', '2.0', '2.5', '2.75',
              '2.85', '3.0', '3.5', '3.75', '3.85', '4.5', '4.75', '4.85', '5.5', '5.75', 
              '5.85', '6.5', '6.75', '6.85', '7.5', '7.75', '7.85', '8.5', '8.75', '8.85',
              '9.5', '9.75', '9.85', '10.5', '11.5', '13.5')
  
  for (i in 1:length(depths)) {
    temp <- field_noon[,c(1, i+1)]
    temp$Depth <- depths[i]
    colnames(temp) <- c('DateTime', 'Temp', 'Depth')
    field_format <- full_join(temp, field_format)
  }
  
  
  # put depth as second column
  field_format <- field_format %>% dplyr::select( 'DateTime', 'Depth', 'Temp') %>% 
    dplyr::arrange(DateTime, Depth)
  field_format <- na.omit(field_format)
  
  buoy <- field_format
  buoy$DateTime <- as.POSIXct(buoy$DateTime)
  buoy$site <- as.character('210') # set up buoy site to 210?
  buoy$method <- 'buoy'
  colnames(buoy) <- c('DateTime', 'Depth', 'Temp_buoy', 'site', 'method')
  buoy <- na.omit(buoy)
  buoy$DateDepth <- paste0(buoy$DateTime, " ", buoy$Depth)
  buoy$Depth <- as.numeric(buoy$Depth)
  
  
  # remove days from buoy dataset where there is manual data
  remove <- manual$DateTime
  
  buoy <- buoy[!buoy$DateTime %in% remove,  ]
  
  # combine the two datasets
  temp_data <- dplyr::full_join(manual, buoy)
  
  data_nodups <- temp_data[!duplicated(temp_data[,1:2]),]
  
  table(duplicated(data_nodups[,1:2]))
  table(duplicated(temp_data[,1:2]))
  
  #ggplot(data_nodups[data_nodups$DateTime > '2018-01-01 00:00:00' & data_nodups$DateTime < '2019-01-01 00:00:00',], aes(x = DateTime, y = Temp_buoy)) +
  #  geom_line() +
  #  geom_line(aes(x= DateTime, y = Temp_manual, col = 'red')) 
  
  
  #ggplot(buoy[buoy$DateDepth > '2018-01-01 00:00:00' & buoy$DateDepth < '2019-01-01 00:00:00',], aes(x = DateTime, y = Temp_buoy)) +
  #  geom_line(aes(col = as.factor(Depth))) 
  
  #ggplot(manual[manual$DateDepth > '2018-01-01 00:00:00' & manual$DateDepth < '2019-01-01 00:00:00',], aes(x = DateTime, y = Temp_manual)) +
  #  geom_line(aes(col = as.factor(Depth))) 
  
  data_nodups <- data_nodups %>% 
    mutate(Temp = ifelse(is.na(Temp_manual), Temp_buoy, Temp_manual)) %>% 
    select(DateTime, Depth, Temp)
  
  write.csv(data_nodups, hist_all_file, row.names = FALSE)
  
  
  # now combine the historical manual + buoy data with the realtime file
  d_head<-read.csv(realtime_file, skip=1, as.is=T) #get header minus wonky Campbell rows
  d <-read.csv(realtime_file, skip=4, header=F) #get data minus wonky Campbell rows
  names(d)<-names(d_head) #combine the names to deal with Campbell logger formatting
  
  #Removes row if the TIMESTAMP column is blank
  d <- d[complete.cases(d$TIMESTAMP),]
  
  #Removes row if the RECORD column has an NA or blank
  d <- d[!(is.na(d$RECORD) | d$RECORD==""), ]
  
  
  # convert to UTC
  #d$TIMESTAMP <- as.POSIXct(d$TIMESTAMP)
  attr(d$TIMESTAMP, "tzone") <- "UTC"
  
  # remove data from before buoy was deployed
  d <- d[d$TIMESTAMP > '2021-06-07 00:00:00',]
  
  # remove days where maintenance occurred
  maint <- gsheet::gsheet2tbl(maintenance_url)
  maint$colnumber <- NA
  maint$TIMESTAMP_start <- as.POSIXct(maint$TIMESTAMP_start)
  maint$TIMESTAMP_end <- as.POSIXct(maint$TIMESTAMP_end)
  attr(maint$TIMESTAMP_start, "tzone") <- "UTC"
  attr(maint$TIMESTAMP_end, "tzone") <- "UTC"
  
  
  for(i in 1:nrow(maint)){
    
    print(i)
    
    # get start and end time of one maintenance event
    start <- maint$TIMESTAMP_start[i]
    end <- maint$TIMESTAMP_end[i]
    if(is.na(end)){
      end <- as.POSIXct(Sys.time())
      attr(end, "tzone") <- "UTC"
    }
    
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
}
  
  # convert depth columns into long format

  temp_format <- data.frame(matrix(ncol=length(variables) + 1), nrow = 0)
  colnames(temp_format) <- c('DateTime', 'Depth', variables)
  #temp_format$DateTime <- as.POSIXct(temp_format$DateTime)
  
  
  depths <- c('0.1', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10')
  
  for (i in 1:length(depths)) {
    temp <- d[,c(1, i+5)]
    #temp$DateTime <- as.POSIXct(temp$DateTime, tryFormats = c("%Y-%m-%d %T", "%Y-%m-%d"))
    temp$Depth <- depths[i]
    colnames(temp) <- c('DateTime', 'Temp', 'Depth')
    temp_format <- full_join(temp, temp_format)
  }
  
  # put depth as second column and sort by date and depth
  temp_format <- temp_format %>% 
    mutate(Depth = as.numeric(Depth)) %>% 
    select( 'DateTime', 'Depth', 'Temp') %>% 
    arrange(DateTime, Depth)
  
  # combine with historical data
  h <- read.csv(hist_all_file)
  #h$DateTime <- as.POSIXct(h$DateTime)
  attr(h$DateTime, "tzone") <- "UTC"
  
  
  dh <- rbind(h, temp_format)
  
  # make some simple QAQC corrections, e.g. if temp > 100C, etc.
  
  # extract midnight observations
  dh <- dh %>%
    dplyr::mutate(date = lubridate::date(DateTime),
                  hour = lubridate::hour(DateTime),
                  depth = Depth) %>% 
    dplyr::filter(hour == 0) %>% 
    dplyr::group_by(date, depth, hour) %>% 
    dplyr::mutate(temperature = mean(Temp, na.rm = TRUE)) %>% # take the average for each hour, data is every ten minutes
    #dplyr::mutate(oxygen = mean(DO, na.rm = TRUE)) %>%   
    dplyr::distinct(date, depth, .keep_all = TRUE)
 
   # take hourly average
  #dh <- dh %>% 
  #  dplyr::mutate(date = lubridate::date(DateTime),
  #         hour = lubridate::hour(DateTime),
  #         depth = Depth) %>% 
  #  dplyr::group_by(date, hour, depth) %>% 
  #  mutate(temperature = mean(Temp, na.rm = TRUE)) %>% # take the average for each hour, data is every ten minutes
  #  distinct(date, hour, depth, .keep_all = TRUE)
  
  # put into FLARE format
  dh <- dh %>% 
    dplyr::select(-c(Depth, Temp)) %>% 
    tidyr::pivot_longer(cols = variables, names_to = 'variable', values_to = 'value') %>% 
    mutate(time = as.POSIXct(DateTime),
           site_id = 'sunp') %>% 
    ungroup() %>% 
    select(time, site_id, depth, value, variable) %>% 
    rename(observed = value)
  
  dh <- na.omit(dh)
  attr(dh$time, "tzone") <- "UTC"
  dh$time <- dh$time - 60*60*4
  
  # quick fix to set all hours to 0 to match with `FLAREr::combine_forecast_observations` function
  #dh$hour <- as.numeric(0)

  if(!dir.exists(dirname(cleaned_insitu_file))){
    dir.create(dirname(cleaned_insitu_file), recursive = TRUE)
  }
  
  write_csv(dh, cleaned_insitu_file)
  
  return(cleaned_insitu_file)
}