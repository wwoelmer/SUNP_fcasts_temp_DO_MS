get_stacked_noaa <- function (lake_directory, config, averaged = TRUE) {
  if (averaged) {
    download_s3_objects(lake_directory, bucket = stringr::str_split_fixed(config$s3$drivers$bucket, 
                                                                          "/", n = 2)[1], prefix = file.path(stringr::str_split_fixed(config$s3$drivers$bucket, 
                                                                                                                                      "/", n = 2)[2], "noaa/NOAAGEFS_1hr_stacked_average", 
                                                                                                             config$location$site_id), region = stringr::str_split_fixed(config$s3$drivers$endpoint, 
                                                                                                                                                                         pattern = "\\.", n = 2)[1], base_url = stringr::str_split_fixed(config$s3$drivers$endpoint, 
                                                                                                                                                                                                                                         pattern = "\\.", n = 2)[2])
  }
  else {
    download_s3_objects(lake_directory, bucket = stringr::str_split_fixed(config$s3$drivers$bucket, 
                                                                          "/", n = 2)[1], prefix = file.path(stringr::str_split_fixed(config$s3$drivers$bucket, 
                                                                                                                                      "/", n = 2)[2], "noaa/NOAAGEFS_1hr_stacked", config$location$site_id), 
                        region = stringr::str_split_fixed(config$s3$drivers$endpoint, 
                                                          pattern = "\\.", n = 2)[1], base_url = stringr::str_split_fixed(config$s3$drivers$endpoint, 
                                                                                                                          pattern = "\\.", n = 2)[2])
  }
}
