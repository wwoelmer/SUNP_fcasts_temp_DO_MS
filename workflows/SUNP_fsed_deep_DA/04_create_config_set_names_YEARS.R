# set up multiple configuration files to initialize for each year
# and allow for sequential updating of process sd 

lake_directory <- here::here()

dir.create(file.path(lake_directory, "configuration", "UC_analysis_2019"))
dir.create(file.path(lake_directory, "configuration", "UC_analysis_2020"))
dir.create(file.path(lake_directory, "configuration", "UC_analysis_2021"))

files_og <- list.files(file.path(lake_directory, "configuration", "UC_analysis"))

file.copy(file.path(lake_directory, "configuration", "UC_analysis", files_og),
          file.path(lake_directory, "configuration", "UC_analysis_2019", files_og),
          recursive = TRUE)

file.copy(file.path(lake_directory, "configuration", "UC_analysis", files_og),
          file.path(lake_directory, "configuration", "UC_analysis_2020", files_og),
          recursive = TRUE)

file.copy(file.path(lake_directory, "configuration", "UC_analysis", files_og),
          file.path(lake_directory, "configuration", "UC_analysis_2021", files_og),
          recursive = TRUE)
