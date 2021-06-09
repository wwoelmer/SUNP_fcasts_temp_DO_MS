# function to download a list of 35-day NOAA forecasts from FLARE s3 bucket

noaa_download_s3 <- function(siteID, # LOWERCASE e.g. sunp
                             date, # start date of noaa forecasts
                             cycle,
                             noaa_horizon, # numeric, either 16 or 35 depending on NOAA forecasts desired
                             noaa_directory # place where forecasts will be downloaded
                             ){
  
  Sys.setenv("AWS_S3_ENDPOINT" = "tacc.jetstream-cloud.org:8080/")
  
  # currently not able to get 35 day forecasts downloaded but the setup is here
  if(noaa_horizon==16){
    end_date <- as.Date(date) + 16
    prefix <- "drivers/noaa-point/NOAAGEFS_1hr"
  }else if(noaa_horizon==35){
    end_date <- as.Date(date) + 35
    end_date_00 <- as.Date(date) + 16
    prefix <- "drivers/noaa/NOAAGEFS_1hr"
  }
  
  ens <- formatC(seq(0, 30), width = 2, flag = 0)
  file_names <- paste0(prefix, "/", siteID, "/", date, "/", cycle, "/NOAAGEFS_1hr_", siteID, "_", date, "T00_", end_date, "T00_ens", ens, ".nc")
  if(noaa_horizon==35){
    file_names[1] <- paste0(prefix, "/", siteID, "/", date, "/", cycle, "/NOAAGEFS_1hr_", siteID, "_", date, "T00_", end_date_00, "T00_ens", ens, ".nc")
  }
  
  #Download a specific file from the server and save it locally (in this example, "localfile.nc"):
  for(i in 1:length(file_names)){
    save_object(region="", 
                file_names[i], 
                file=paste0(noaa_directory, file_names[i]), 
                bucket="flare")
  }  
  
}




