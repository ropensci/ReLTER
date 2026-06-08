# Use official R base image
FROM rocker/rstudio:4.3.1

LABEL org.opencontainers.image.authors="Alessandro Oggioni <alessandro.oggioni@cnr.it>"

# Non-interactive installation
ENV DEBIAN_FRONTEND=noninteractive
ARG NCPUS=-2

# Install system libraries needed by R packages
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libfontconfig1-dev \
    libjq-dev \
    libv8-dev \
    libgit2-dev \
    build-essential \
    gfortran \
    libglpk-dev \
    git \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install CRAN packages in smaller groups to reduce memory usage
RUN install2.r --error --skipinstalled -n $NCPUS \
    dplyr dtplyr ggforce ggspatial geojsonsf

RUN install2.r --error --skipinstalled -n $NCPUS \
    ggplot2 httr2 jqr jsonlite leaflet

RUN install2.r --error --skipinstalled -n $NCPUS \
    lifecycle lubridate magrittr purrr qrcode

RUN install2.r --error --skipinstalled -n $NCPUS \
    sf utils units tibble stringr terra xml2

# Install Suggested packages in smaller groups
RUN install2.r --error --skipinstalled -n $NCPUS \
    cowplot geodata httptest2 ISOcodes knitr

RUN install2.r --error --skipinstalled -n $NCPUS \
    leaflet.extras prettymapr RColorBrewer shiny spocc

RUN install2.r --error --skipinstalled -n $NCPUS \
    tidyr taxize testthat withr worrms
    
# Install remotes (needed to install zen4R from GitHub)
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/', dependencies = TRUE)"

# Install ReLTER package from GitHub
RUN R -e "remotes::install_github('ropensci/ReLTER', ref='main', lib=.Library, dependencies=FALSE)"

# Ensure the 'rstudio' user has read/write permissions on the system library
# so that installed packages (like ReLTER) are accessible in RStudio
RUN chown -R rstudio:rstudio /usr/local/lib/R/site-library

# Set default working directory
WORKDIR /home/rstudio