FROM rocker/geospatial:4.1.2

LABEL org.opencontainers.image.authors="Paolo Tagliolato <ptagliolato@irea.cnr.it>"
COPY add_ReLTER.sh /rocker_scripts/add_ReLTER.sh
RUN chown root:root /rocker_scripts/add_ReLTER.sh
RUN chmod +x /rocker_scripts/add_ReLTER.sh
RUN /rocker_scripts/add_ReLTER.sh
