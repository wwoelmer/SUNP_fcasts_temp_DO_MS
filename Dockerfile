FROM flareforecast/flare

ENV NB_USER=rstudio

RUN apt-get update && \
    apt-get -y install libzmq5 && \
    apt-get purge && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN /rocker_scripts/install_python.sh
RUN /rocker_scripts/install_binder.sh

EXPOSE 8888

CMD jupyter notebook --ip 0.0.0.0

USER ${NB_USER}

WORKDIR /home/${NB_USER}
