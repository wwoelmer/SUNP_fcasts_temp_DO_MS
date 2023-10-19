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


##### download scores files
scores <- "https://zenodo.org/records/10016133/files/scores.zip?download=1"
scores_dest <- file.path(lake_directory, 'scores', site_id, sim_name, "scores.zip")
download.file(url = scores, destfile = scores_dest)
unzip(scores_dest, exdir = file.path(lake_directory, 'scores', site_id, sim_name))
unlink(scores_dest)

##### download scores files for climatology forecasts
scores <- "https://zenodo.org/records/10016133/files/climatology_scores.csv?download=1"
scores_dest <- file.path(lake_directory, 'scores', site_id, "climatology_scores.csv")
download.file(url = scores, destfile = scores_dest)

##### download scores files for random walk (aka persistence) forecasts
scores <- "https://zenodo.org/records/10016133/files/RW_scores.csv?download=1"
scores_dest <- file.path(lake_directory, 'scores', site_id, "RW_scores.csv")
download.file(url = scores, destfile = scores_dest)

##### download target observations
targets <- "https://zenodo.org/records/10016133/files/sunp-targets-insitu.csv?download=1"
scores_dest <- file.path(lake_directory, 'targets', site_id, sim_name, "sunp-targets-insitu.csv")
download.file(url = scores, destfile = scores_dest)

##### download forecast files
###### NOTE: this takes a long time! #######

fcasts <- "https://zenodo.org/records/10016133/files/forecasts.zip?download=1"
fcast_dest <- file.path(lake_directory, 'forecasts', site_id, sim_name, "fcasts.zip")
download.file(url = fcasts, destfile = fcast_dest,
              method = "curl")
unzip(fcast_dest, exdir = file.path(lake_directory, 'forecasts', site_id, sim_name))
unlink(fcast_dest)

