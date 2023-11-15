# forecast simulation for July 17 LSPA meeting

############## set up config directories
lake_directory <- getwd() # Captures the project directory 
config <- yaml::read_yaml(file.path(lake_directory,"configuration", "FLAREr", "configure_flare.yml"))

# Set working directories for your system
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config$file_path$noaa_directory <- file.path(lake_directory, "forecasted_drivers")
config_obs <- yaml::read_yaml(file.path(lake_directory,"configuration", "observation_processing", "observation_processing.yml"))

# set up run config settings
run_config <- yaml::read_yaml(file.path(lake_directory,"configuration", "FLAREr", "configure_run.yml"))
config$run_config <- run_config

# run forecast 35 days before July 17
config$run_config$forecast_start_datetime <- '2021-06-12 16:00:00'
config$run_config$sim_name <- "35_days_out_updatedNOAA"

#source(file.path(lake_directory, '01_get_data.R'))
source(file.path(lake_directory, '02_process_data.R'))
source(file.path(lake_directory, '03_generate_forecast.R'))
source(file.path(lake_directory, '04_visualize.R'))

# run 21 days before July 17
config$run_config$forecast_start_datetime <- '2021-06-19 16:00:00'
config$run_config$sim_name <- "21_days_out"

#source(file.path(lake_directory, '01_get_data.R'))
#source(file.path(lake_directory, '02_process_data.R'))
source(file.path(lake_directory, '03_generate_forecast.R'))
source(file.path(lake_directory, '04_visualize.R'))

# run 14 days before July 17
config$run_config$forecast_start_datetime <- '2021-07-03 16:00:00'
config$run_config$sim_name <- "14_days_out_updatedNOAA"
#config$run_config$forecast_horizon <- '16'

source(file.path(lake_directory, '03_generate_forecast.R'))
source(file.path(lake_directory, '04_visualize.R'))

# run 14 days before July 17
config$run_config$forecast_start_datetime <- '2021-07-03 16:00:00'
config$run_config$end_datetime <- '2021-06-28'
config$run_config$sim_name <- "14_days_out_updatedNOAA_endDAearly"
#config$run_config$forecast_horizon <- '16'

source(file.path(lake_directory, '03_generate_forecast.R'))
source(file.path(lake_directory, '04_visualize.R'))

# run 7 days before July 17
config$run_config$forecast_start_datetime <- '2021-07-10 16:00:00'
config$run_config$start_datetime <- '2021-06-08'
config$run_config$sim_name <- "7_days_out_updatedNOAA"
#config$run_config$forecast_horizon <- '16'

#source(file.path(lake_directory, '01_get_data.R'))
source(file.path(lake_directory, '02_process_data.R'))
source(file.path(lake_directory, '03_generate_forecast.R'))
source(file.path(lake_directory, '04_visualize.R'))

# run 7 days before July 17 with more ensembles
config$run_config$forecast_start_datetime <- '2021-07-10 16:00:00'
config$run_config$start_datetime <- '2021-06-08'
config$run_config$sim_name <- "7_days_out_updatedNOAA_250ens"
config$da_setup$ensemble_size <- 220
#config$run_config$forecast_horizon <- '16'

#source(file.path(lake_directory, '01_get_data.R'))
#source(file.path(lake_directory, '02_process_data.R'))
source(file.path(lake_directory, '03_generate_forecast.R'))
source(file.path(lake_directory, '04_visualize.R'))

# run 1 day before July 17
config$run_config$forecast_start_datetime <- '2021-07-15 16:00:00'
config$run_config$sim_name <- "2_days_out"
config$run_config$forecast_horizon <- '35'

source(file.path(lake_directory, '01_get_data.R'))
source(file.path(lake_directory, '02_process_data.R'))
source(file.path(lake_directory, '03_generate_forecast.R'))
source(file.path(lake_directory, '04_visualize.R'))

# run 1 day before July 17
config$run_config$forecast_start_datetime <- '2021-07-15 16:00:00'
config$run_config$start_datetime <- '2021-07-06'
config$run_config$sim_name <- "2_days_out_shortDA"
config$run_config$forecast_horizon <- '35'

source(file.path(lake_directory, '03_generate_forecast.R'))
source(file.path(lake_directory, '04_visualize.R'))

