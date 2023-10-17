Repo for downloading, running, and analyzing forecasts of water temperature and dissolved oxygen at Lake Sunapee for TITLE MANUSCRIPT submitted to JOURNAL on DATE.

The workflow begins by first running install.R to install all necessary packages. Then, access the workflow scripts for downloading and running forecasts within workflows/SUNP_fcasts_temp_DO.

Forecast scores and raw ensemble forecast files can be downloaded from a published Zenodo repository within 01_download_forecasts.R. The climatology and persistence null forecasts can be run within 02_climatology_null.R and 03_random_walk_null.R, respectively. 