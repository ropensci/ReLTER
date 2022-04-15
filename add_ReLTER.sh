#!/bin/bash
set -e

# always set this for scripts but don't declare as ENV..
export DEBIAN_FRONTEND=noninteractive

## build ARGs
NCPUS=${NCPUS:--1}

#apt-get update -qq \
#  && apt-get install -y --no-install-recommends \
#  libjq-dev

install2.r --error --skipinstalled -n $NCPUS \
	jqr \
	leaflet \
	qrcode \
	taxize \
	waffle \
	worrms \
	xslt \
	spocc \
	ggforce \
	rosm \
	MODIStsp

R -e "devtools::install_github('https://github.com/oggioniale/ReLTER',ref = 'main',dependencies = FALSE)"
#R -e "devtools::install_github('https://github.com/oggioniale/ReLTER',ref = 'dev__withImprovements',dependencies = FALSE)"
