# download forecasts and scores from zenodo

lake_directory <- here::here()
sim_name <- "all_UC_fsed_deep_DA"
site_id <- "sunp"

##### set up directories
# for scores
dest <- file.path(lake_directory, 'scores')
dir.create(dest)

dest <- file.path(lake_directory, 'scores', site_id)
dir.create(dest)

dest <- file.path(lake_directory, 'scores', site_id, sim_name)
dir.create(dest)

# for forecasts
dest <- file.path(getwd(), 'forecasts', site_id)
dir.create(dest)

dest <- file.path(getwd(), 'forecasts', site_id, sim_name)
dir.create(dest)


##### download scores files
scores <- "https://zenodo.org/record/8212703/files/scores.zip?download=1"
scores_dest <- file.path(lake_directory, 'scores', site_id, sim_name, "scores.zip")
download.file(url = scores, destfile = scores_dest,
              method = "curl")
unzip(scores_dest, exdir = file.path(lake_directory, 'scores', site_id, sim_name))

##### download forecast files
###### NOTE: this takes a long time! #######

fcasts <- "https://zenodo.org/record/8212703/files/forecasts.zip?download=1"
fcast_dest <- file.path(lake_directory, 'forecasts', site_id, sim_name, "fcasts.zip")
download.file(url = fcasts, destfile = fcast_dest,
              method = "curl")
unzip(fcast_dest, exdir = file.path(lake_directory, 'forecasts', site_id, sim_name))

