# plot EXO DO + proDSS DO to see mismatch

d_head<-read.csv('./data_raw/buoy-data/SUNP_buoy_wq.csv', skip=1, as.is=T) #get header minus wonky Campbell rows
d <-read.csv('./data_raw/buoy-data/SUNP_buoy_wq.csv', skip=4, header=F) #get data minus wonky Campbell rows
names(d)<-names(d_head) #combine the names to deal with Campbell logger formatting

#Removes row if the TIMESTAMP column is blank
d <- d[complete.cases(d$TIMESTAMP),]

#Removes row if the RECORD column has an NA or blank
d <- d[!(is.na(d$RECORD) | d$RECORD==""), ]


# remove data from before buoy was deployed
d <- d[d$TIMESTAMP > '2021-06-07 00:00:00',]

# remove days where maintenance occurred
maintenance_url <- 'https://docs.google.com/spreadsheets/d/1IfVUlxOjG85S55vhmrorzF5FQfpmCN2MROA_ttEEiws/edit?usp=sharing'
maint <- gsheet::gsheet2tbl(maintenance_url)
maint$colnumber <- NA
maint$TIMESTAMP_start <- as.POSIXct(maint$TIMESTAMP_start)
maint$TIMESTAMP_end <- as.POSIXct(maint$TIMESTAMP_end)
attr(maint$TIMESTAMP_start, "tzone") <- "UTC"
attr(maint$TIMESTAMP_end, "tzone") <- "UTC"


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
}


dh <- d %>% select(TIMESTAMP, wtr_1, wtr_10, dosat_1, doobs_1, dotemp, dosat, doobs)
dh$TIMESTAMP <- as.POSIXct(dh$TIMESTAMP)
#dh <- dh[dh$dosat_1 > 5,]

manual_url <- 'https://docs.google.com/spreadsheets/d/1IfVUlxOjG85S55vhmrorzF5FQfpmCN2MROA_ttEEiws/edit#gid=1721211942'
manual <- gsheet::gsheet2tbl(manual_url)
manual$Date <- as.POSIXct(manual$Date) - 60*60*8
manual <- manual[manual$Site=='buoy',]

ggplot(data = dh, aes (x = TIMESTAMP, y = dosat_1, col = '1m')) + 
  geom_line() + 
  geom_line(aes(x = TIMESTAMP, y = dosat, col = '10m')) +
  geom_point(data = manual[manual$Depth==1,], aes(x = Date, y = DO_Sat, col = '1m'), size = 4) +
  geom_point(data = manual[manual$Depth==10,], aes(x = Date, y = DO_Sat, color = '10m'), size = 4)

ggplot(data = dh, aes (x = TIMESTAMP, y = doobs_1, col = '1m')) + 
  geom_line() + 
  geom_line(aes(x = TIMESTAMP, y = doobs, col = '10m')) +
  geom_point(data = manual[manual$Depth==1,], aes(x = Date, y = DO_mgL, col = '1m'), size = 4) +
  geom_point(data = manual[manual$Depth==10,], aes(x = Date, y = DO_mgL, color = '10m'), size = 4)

maint_days <- maint[maint$instrument=='EXO',]
maint_days <- as.Date(maint_days$TIMESTAMP_start)

day <- dh[dh$TIMESTAMP>as.POSIXct('2021-07-13') & dh$TIMESTAMP<as.POSIXct('2021-07-15'),]
ggplot(data = day, aes(x = TIMESTAMP, y = dosat_1, col = '1m')) + 
  geom_line() + 
  #geom_line(aes(x = TIMESTAMP, y = dosat, col = '10m')) + 
  geom_vline(xintercept = as.POSIXct(maint_days))
