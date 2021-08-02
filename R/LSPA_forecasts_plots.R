# reprocess output figures for LSPA annual meeting for cayelan


source(file.path(lake_directory,"R/simple_plot.R"))

highlight_date <- as.Date('2021-07-17')
focal_depths_plotting <- c('0.1', '5', '10')

# 35 days out forecast
saved_file <- file.path(config$file_path$forecast_output_directory, "35_days_out_H_2021_06_08_2021_06_12_F_35_20210712T113424.nc")
forecast_file_name <- saved_file
output_file_name <- file.path(config$file_path$forecast_output_directory,  'LSPA_figs_for_Cayelan', '35_days_out')
qaqc_data_directory <- config$file_path$qaqc_data_directory

simple_plot(forecast_file_name,
            output_file_name,
            qaqc_data_directory,
            focal_depths_plotting,
            highlight_date = highlight_date)

#21 days out forecast
saved_file <- file.path(config$file_path$forecast_output_directory, "21_days_out_H_2021_06_08_2021_06_19_F_35_20210712T171456.nc")
forecast_file_name <- saved_file
output_file_name <- file.path(config$file_path$forecast_output_directory,  'LSPA_figs_for_Cayelan', '21_days_out')
qaqc_data_directory <- config$file_path$qaqc_data_directory

simple_plot(forecast_file_name,
            output_file_name,
            qaqc_data_directory,
            focal_depths_plotting,
            highlight_date = highlight_date)

#14 days out forecast
saved_file <- file.path(config$file_path$forecast_output_directory, "14_days_out_H_2021_06_08_2021_07_03_F_35_20210713T150158.nc")
forecast_file_name <- saved_file
output_file_name <- file.path(config$file_path$forecast_output_directory,  'LSPA_figs_for_Cayelan', '14_days_out')
qaqc_data_directory <- config$file_path$qaqc_data_directory

simple_plot(forecast_file_name,
            output_file_name,
            qaqc_data_directory,
            focal_depths_plotting,
            highlight_date = highlight_date)

#7 days out forecast
saved_file <- file.path(config$file_path$forecast_output_directory, "7_days_out_H_2021_06_08_2021_07_10_F_35_20210714T181508.nc")
forecast_file_name <- saved_file
output_file_name <- file.path(config$file_path$forecast_output_directory,  'LSPA_figs_for_Cayelan', '7_days_out')
qaqc_data_directory <- config$file_path$qaqc_data_directory

simple_plot(forecast_file_name,
            output_file_name,
            qaqc_data_directory,
            focal_depths_plotting,
            highlight_date = highlight_date)

