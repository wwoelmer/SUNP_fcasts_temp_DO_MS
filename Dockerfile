FROM rocker/geospatial:4.1.2 

RUN apt-get update && apt-get -y install libgd-dev libnetcdf-dev git

USER rstudio

RUN git clone  https://github.com/wwoelmer/SUNP-forecast-code.git && cd /home/rstudio/SUNP-forecast-code && git checkout glm_aed

RUN Rscript /home/rstudio/SUNP-forecast-code/01_install_packages.R

USER root
