# FLARE forecasts at Lake Sunapee, NH, USA

Repo for downloading, running, and analyzing forecasts of water temperature and dissolved oxygen at Lake Sunapee, NH, USA for TITLE MANUSCRIPT submitted to JOURNAL on DATE.

## Reproduce manuscript figures

1. Dowload or clone the github repository to your local computer

2. The workflow begins by first running `01_install_packages.R` to install all necessary packages. 

3. Then, run `02_download_files.R` to download published forecasts, scores files, target observations, and meteorological driver files from Woelmer et al. 2023.

4. Run the files within the `analysis` folder to produce the manuscript and Supplementary Information figures.

## Run forecast and scoring workflow

1. Download or clone the github repository to your local computer.

2. The workflow begins by first running `01_install_packages.R` to install all necessary packages. 

3. Run `workflows/SUNP_fcasts_temp_DO/02_climatology_null.R` to reproduce the climatology forecasts.

4. Run `workflows/SUNP_fcasts_temp_DO/03_random_walk_null.R` to reproduce the random-wallk (persistence) forecasts.

5. Run `workflows/SUNP_fcasts_temp_DO/04capped_fsed_deep_DA_2021.R` to reproduce the FLARE temperature and dissolved oxygen forecasts for 2021.

6. Run `workflows/SUNP_fcasts_temp_DO/04capped_fsed_deep_DA_2022.R` to reproduce the FLARE temperature and dissolved oxygen forecasts for 2022.

7. Run the files within the `analysis` folder to produce the manuscript and Supplementary Information figures.

# 

Woelmer WM, Thomas RQ, Olsson F, et al (2023) Forecasts, score summary files, target observational data, and meteorological driver files to accompany the manuscript "Skill of process-based forecasts relative to null models varies across time and depth for water temperature and dissolved oxygen" (1.3) [Data set]. Zenodo. <https://doi.org/10.5281/zenodo.10127798>
