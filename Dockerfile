FROM flareforecast/flare

ENV NB_USER=rstudio

RUN R --quiet -e "devtools::install_github('IRkernel/IRkernel')" && \
    R --quiet -e "IRkernel::installspec(prefix='${VENV_DIR}')"

RUN /rocker_scripts/install_python.sh
RUN /rocker_scripts/install_binder.sh

EXPOSE 8888

CMD jupyter notebook --ip 0.0.0.0

USER ${NB_USER}

WORKDIR /home/${NB_USER}
