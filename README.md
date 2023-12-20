ReLTER
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- other badges https://github.com/GuangchuangYu/badger -->

[![](https://badges.ropensci.org/485_status.svg)](https://github.com/ropensci/software-review/issues/485)
[![](https://img.shields.io/badge/doi-10.5281/zenodo.5576813-yellow.svg)](https://doi.org/10.5281/zenodo.5576813)
[![r-universe](https://ropensci.r-universe.dev/badges/ReLTER)](https://ropengov.r-universe.dev/)
[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
[![](https://img.shields.io/github/languages/code-size/ropensci/ReLTER.svg)](https://github.com/ropensci/ReLTER)
[![](https://img.shields.io/github/last-commit/ropensci/ReLTER.svg)](https://github.com/ropensci/ReLTER/commits/main)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R build
status](https://github.com/rossellhayes/ipa/workflows/R-CMD-check/badge.svg)](https://github.com/rossellhayes/ipa/actions)
[![](https://img.shields.io/badge/devel%20version-2.2.0-blue.svg)](https://github.com/ropensci/ReLTER)
[![codecov](https://codecov.io/gh/ropensci/ReLTER/branch/dev/graph/badge.svg)](https://codecov.io/gh/ropensci/ReLTER)
<!-- CRAN badges -->
<!-- [![](https://www.r-pkg.org/badges/version/ropensci/ReLTER?color=orange)](https://cran.r-project.org/package=ropensci/ReLTER) -->
<!-- [![CRAN checks](https://badges.cranchecks.info/summary/ReLTER.svg)](https://cran.r-project.org/web/checks/check_results_ReLTER.html) -->
<!-- [![](http://cranlogs.r-pkg.org/badges/last-month/badger?color=green)](https://cran.r-project.org/package=badger) -->
<!-- badges: end -->

`{ReLTER}` is an R package that provides access to
[DEIMS-SDR](https://deims.org/), allowing to interact with software
implemented by eLTER Research Infrastructure (RI) and improving the
data/information shared among the Long Term Ecological Research (LTER)
network. This package was conceived within eLTER H2020 project and will
help advance the development of European Long-Term Ecosystem Research
Infrastructures ([eLTER RI](https://elter-ri.eu)).

The `{ReLTER}` package functions in particular allows to:

- retrieve the information about entities (e.g. sites, datasets, and
  activities) shared by [DEIMS-SDR](https://deims.org/) (see e.g.
  [get_site_info
  function](https://docs.ropensci.org/ReLTER/reference/get_site_info.html));

- elaborate the information of single site or merge info from national
  network sites or entire International LTER (ILTER) in order to provide
  maps, figures, graphs etc (see e.g.
  [get_network_sites](https://docs.ropensci.org/ReLTER/reference/get_network_sites.html),
  [produce_site_map](https://docs.ropensci.org/ReLTER/reference/produce_site_map.html)
  or
  [produce_site_observedProperties_pie](https://docs.ropensci.org/ReLTER/reference/produce_site_observedProperties_pie.html)
  functions);

- interact with the [ODSEurope](maps.opendatascience.eu) managed by
  members of the
  [Geo-harmonizer](https://opendatascience.eu/geoharmonizer-project/)
  project starting with the dataset shared by
  [DEIMS-SDR](https://deims.org/) (see e.g.
  [get_site_ODS](https://docs.ropensci.org/ReLTER/reference/get_site_ODS.html)
  function);

- improve the quality of the dataset (see e.g.
  [get_id_worms](https://docs.ropensci.org/ReLTER/reference/get_id_worms.html)).

Functions currently implemented are derived from the discussion of the
needs declared by eLTER users community.

The `{ReLTER}` package will follow the progress of eLTER-RI
infrastructure and evolve with improvements and development of new
tools.

<!-- about the icons https://github.com/ikatyang/emoji-cheat-sheet -->

## :notebook_with_decorative_cover: Citation

To cite `{ReLTER}` please use: Alessandro Oggioni, Micha Silver, Luigi
Ranghetti & Paolo Tagliolato. (2023). ReLTER: An Interface for the eLTER
Community (v2.2). Zenodo. <https://doi.org/10.5281/zenodo.5576813>

or:

``` bibtex
@software{alessandro_oggioni_2021_5576813,
  author       = {Alessandro Oggioni and Micha Silver and Luigi Ranghetti and Paolo Tagliolato},
  title        = {ReLTER: An Interface for the eLTER Community},
  month        = jan,
  year         = 2023,
  publisher    = {Zenodo},
  version      = {v2.2},
  doi          = {10.5281/zenodo.5576813},
  url          = {https://doi.org/10.5281/zenodo.5576813}
}
```

## :book: Documentation

Visit the `{ReLTER}` website at
[docs.ropensci.org/ReLTER/](https://docs.ropensci.org/ReLTER/) for
further documentation, examples, and installation of the package.

The manual of `{ReLTER}` package could be found
[here](https://ropensci.r-universe.dev/manual/ReLTER.pdf).

## :arrow_double_down: Installation

You can install the main version of `{ReLTER}` from
[GitHub](https://github.com/ropensci/ReLTER) with:

    install.packages("devtools")
    devtools::install_github("ropensci/ReLTER")
    library(ReLTER)

If you want to install different package branch (e.g. ‘dev’) can you use
this command:

    devtools::install_github('https://github.com/ropensci/ReLTER', ref = 'dev')

The `{ReLTER}` package is part of the
[R-universe](https://r-universe.dev/) community and it can be installed
also use this command:

    utils::install.packages("ReLTER", repos = "https://ropensci.r-universe.dev")

Alternatively {`ReLTER`} can be used [in a Docker
container](./articles/rocker_ReLTER.html).

If you wish to help develop this package, please follow the
[contributing guidelines](CONTRIBUTING.md).

## :woman_technologist: Persons involved :man_technologist:

Alessandro Oggioni <https://orcid.org/0000-0002-7997-219X> (CNR,
Institute for Electromagnetic Sensing of the Environment - IREA)

<!-- Add authors, reviewers including those who, within eLTER network, reviewed the package and add acknowledgements 
     in accordance with this https://ropensci.org/blog/2018/03/16/thanking-reviewers-in-metadata/ and this
     https://devguide.ropensci.org/building.html#authorship.
     Add the same in DESCRIPTION file -->

Micha Silver <https://orcid.org/0000-0002-1128-1325> (Ben Gurion
University - BGU)

Luigi Ranghetti <https:://orcid.org/0000-0001-6207-5188> (CNR, Institute
for Electromagnetic Sensing of the Environment - IREA)

Paolo Tagliolato <https:://orcid.org/0000-0002-0261-313X> (CNR,
Institute for Electromagnetic Sensing of the Environment - IREA)

For a exhaustive list of contributors please visit [authors
page](https://docs.ropensci.org/ReLTER/authors).

## :office: Contributing organizations

<img src="man/figures/irea_logo.png" height="72" alt="CNR-IREA" />
<!--a href="http://www.irea.cnr.it/en/"><img src="man/figures/irea_logo.png" height="40" align="left" /></a-->

<img src="man/figures/bgu_logo.png" height="80" alt="BGU" />
<!--a href="https://in.bgu.ac.il/en/"><img src="man/figures/bgu_logo.png" height="40" align="left" /></a-->

## :thumbsup: Acknowledgements

This work has been partially funded from the European Union’s Horizon
2020 research and innovation programme under the [H2020 eLTER-Plus
Project](https://elter-ri.eu/elter-plus) grant agreement No 871128.

Thanks to the reviewers and the editor ([more about
authors](https://docs.ropensci.org/ReLTER/authors.html)) for their work.
