FROM flareforecast/flare

ENV NB_USER=rstudio

RUN /rocker_scripts/install_python.sh
RUN /rocker_scripts/install_binder.sh

EXPOSE 8888

CMD jupyter notebook --ip 0.0.0.0

USER ${NB_USER}

WORKDIR /home/${NB_USER}