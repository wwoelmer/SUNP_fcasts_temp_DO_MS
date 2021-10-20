#saved_file <- "/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/run_flare_package/wrr_runs_obs/wrr_runs_H_2019_03_14_2019_03_15_F_16_20201124T085027.nc"
#qaqc_data_location <- "/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE_package/flare_lake_examples/fcre/qaqc_data/"
lake_directory <- here::here()
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$forecast_output_directory <- file.path(dirname(lake_directory), "forecasts", forecast_site)


#file_name <- saved_file
FLAREr::plotting_general(file_name = saved_file,qaqc_data_directory = config$file_path$qaqc_data_directory, ncore = 2)

source(file.path(lake_directory,"R/simple_plot.R"))

forecast_file_name <- saved_file
output_file_name <- paste0(config$file_path$forecast_output_directory, "/", config$run_config$sim_name, "_", lubridate::date(config$run_config$forecast_start_datetime), "_", config$run_config$forecast_horizon, '_day_forecast_simple_plot' )
qaqc_data_directory <- config$file_path$qaqc_data_directory
focal_depths_plotting <- c('0.1', '5', '10')
highlight_date <- as.Date('2021-07-17')

simple_plot(forecast_file_name,
            output_file_name,
            qaqc_data_directory,
            focal_depths_plotting,
            highlight_date = highlight_date)


