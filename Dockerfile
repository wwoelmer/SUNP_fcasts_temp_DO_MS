FROM rocker/geospatial:4.1.2

RUN apt-get update && apt-get -y install libgd-dev libnetcdf-dev git

USER rstudio

RUN git clone https://github.com/wwoelmer/SUNP-forecast-code.git /home/rstudio/SUNP-forecast-code

RUN Rscript /home/rstudio/SUNP-forecast-code/01_install_packages.R
