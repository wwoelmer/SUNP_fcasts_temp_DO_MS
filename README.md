# FLARE forecasts at Lake Sunapee, NH, USA

This repository contains code for downloading, running, and analyzing forecasts of water temperature and dissolved oxygen at Lake Sunapee, NH, USA for the manuscript "Process-based forecasts of lake water temperature and dissolved oxygen outperform null models, with variability over time and depth". Forecasts are run using the FLARE (Forecasting Lake and Reservoir Ecosystem) framework. Contact Whitney Woelmer (wwoelmer@vt.edu) with any questions.

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

## Instructions for reproducing using Docker

1. Download and install Docker to your computer (https://www.docker.com)

2. At the command line in Rstudio, run `docker run --rm -ti -e PASSWORD=yourpassword -p 8787:8787 wwoelmer/wwoelmer_et_al:latest`

3. Open a webbrowser and enter http://localhost:8787. You will see an Rstudio login screen. The user name is rstudio and the password is yourpassword

4. In the Rstudio session: File -> Open project -> select SUNP_fcasts_temp_DO_MS/SUNP_fcasts_temp_DO_MS.Rproj

5. Follow the instructions above for reproducing the figures or the forecasts (note: the R packages are already installed in the Docker container so 01_install_packages.R does not need to be run)

# 
Woelmer WM, Thomas RQ, Olsson F, et al (2023) Forecasts, score summary files, target observational data, and meteorological driver files to accompany the manuscript "Process-based forecasts of lake water temperature and dissolved oxygen outperform null models, with variability over time and depth" [Data set]. Zenodo. <https://doi.org/10.5281/zenodo.10127798>
