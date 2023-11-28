FROM rocker/geospatial:4.1.2 

RUN apt-get update && apt-get -y install libgd-dev libnetcdf-dev git

USER rstudio

RUN git clone https://github.com/wwoelmer/SUNP_fcasts_temp_DO_MS.git#main /home/rstudio/SUNP_fcasts_temp_DO_MS

RUN Rscript /home/rstudio/SUNP_fcasts_temp_DO_MS/01_install_packages.R

USER root
