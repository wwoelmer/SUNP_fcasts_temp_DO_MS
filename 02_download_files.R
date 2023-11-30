# download forecasts and scores from zenodo

lake_directory <- here::here()
sim_name <- "SUNP_fcasts_temp_DO"
site_id <- "sunp"

##### set up directories
# for scores
dest <- file.path(lake_directory, 'scores', site_id, sim_name)
dir.create(dest, recursive = TRUE)

# for forecasts
dest <- file.path(getwd(), 'forecasts', site_id, sim_name)
dir.create(dest, recursive = TRUE)

# for targets
dest <- file.path(getwd(), 'targets', site_id, sim_name)
dir.create(dest, recursive = TRUE)


##### download FLARE score files
# published on Zenodo at Woelmer WM, Thomas RQ, Olsson F, et al (2023) https://doi.org/10.5281/zenodo.8212702
scores <- "https://zenodo.org/records/10223812/files/forecasts.zip?download=1"
scores_dest <- file.path(lake_directory, 'scores', site_id, sim_name, "scores.zip")
download.file(url = scores, destfile = scores_dest)
unzip(scores_dest, exdir = file.path(lake_directory, 'scores', site_id, sim_name))
unlink(scores_dest)

##### download scores files for climatology forecasts
scores <- "https://zenodo.org/records/10223812/files/climatology_scores.csv?download=1"
scores_dest <- file.path(lake_directory, 'scores', site_id, "climatology_scores.csv")
download.file(url = scores, destfile = scores_dest)

##### download scores files for random walk (aka persistence) forecasts
scores <- "https://zenodo.org/records/10223812/files/RW_scores.csv?download=1"
scores_dest <- file.path(lake_directory, 'scores', site_id, "RW_scores.csv")
download.file(url = scores, destfile = scores_dest)

##### download target observations
targets <- "https://zenodo.org/records/10223812/files/sunp-targets-insitu.csv?download=1"
targets_dest <- file.path(lake_directory, 'targets', site_id, sim_name, "sunp-targets-insitu.csv")
download.file(url = targets, destfile = targets_dest)

##### download met driver data
options(timeout = 60*5) # set longer timeout to five minutes so the download can complete
met <- "https://zenodo.org/records/10223812/files/drivers.zip?download=1"
met_dest <- file.path(lake_directory, "drivers.zip")
download.file(url = met, destfile = met_dest)
unzip(met_dest)
unlink(met_dest)

# move folder to follow file structure expected by FLARE
dir.create(file.path(lake_directory, 'drivers/noaa/gefs-v12-reprocess/stage3/parquet/sunp'), recursive = T)
file.copy(from = file.path(lake_directory, 'drivers/noaa/gefs-v12-reprocess/stage3/sunp/part-0.parquet'),
          to = file.path(lake_directory, 'drivers/noaa/gefs-v12-reprocess/stage3/parquet/sunp/part-0.parquet'),
          overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)

##### download forecast files
###### NOTE: this takes a long time! and is not needed to reproduce manuscript plots #######

get_forecasts <- FALSE

if (get_forecasts == T) {
  fcasts <- "https://zenodo.org/records/10223812/files/forecasts.zip?download=1"
  fcast_dest <- file.path(lake_directory, 'forecasts', site_id, sim_name, "fcasts.zip")
  download.file(url = fcasts, destfile = fcast_dest,
                method = "curl")
  unzip(fcast_dest, exdir = file.path(lake_directory, 'forecasts', site_id, sim_name))
  unlink(fcast_dest)  
}


