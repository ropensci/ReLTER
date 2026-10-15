# ReLTER

[ReLTER](https://docs.ropensci.org/ReLTER) is an R package that provides
access to [DEIMS-SDR](https://deims.org/), allowing to interact with
software implemented by eLTER Research Infrastructure (RI) and improving
the data/information shared among the Long Term Ecological Research
(LTER) network. This package was conceived within eLTER H2020 project
and will help advance the development of European Long-Term Ecosystem
Research Infrastructures ([eLTER RI](https://elter-ri.eu)).

The [ReLTER](https://docs.ropensci.org/ReLTER) package functions in
particular allows to:

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

- interact with the
  [ODSEurope](https://docs.ropensci.org/ReLTER/maps.opendatascience.eu)
  managed by members of the
  [Geo-harmonizer](https://opendatascience.eu/geoharmonizer-project/)
  project starting with the dataset shared by
  [DEIMS-SDR](https://deims.org/) (see e.g.
  [get_site_ODS](https://docs.ropensci.org/ReLTER/reference/get_site_ODS.html)
  function);

- improve the quality of the dataset (see e.g.
  [get_id_worms](https://docs.ropensci.org/ReLTER/reference/get_id_worms.html)).

Functions currently implemented are derived from the discussion of the
needs declared by eLTER users community.

The [ReLTER](https://docs.ropensci.org/ReLTER) package will follow the
progress of eLTER-RI infrastructure and evolve with improvements and
development of new tools.

## 📔 Citation

To cite [ReLTER](https://docs.ropensci.org/ReLTER) please use:

Oggioni, A., Silver, M., Tagliolato, P., & Karnieli, A. (2025). ReLTER:
An R interface for environmental observation in long term ecological
research. Ecological Informatics, 85, 102915.
<https://doi.org/10.1016/j.ecoinf.2024.102915>

``` bibtex
@article{OGGIONI2025102915,
title = {ReLTER: An R interface for environmental observation in long term ecological research},
journal = {Ecological Informatics},
volume = {85},
pages = {102915},
year = {2025},
issn = {1574-9541},
doi = {https://doi.org/10.1016/j.ecoinf.2024.102915},
url = {https://www.sciencedirect.com/science/article/pii/S1574954124004576},
author = {Alessandro Oggioni and Micha Silver and Paolo Tagliolato and Arnon Karnieli}
```

or:

Alessandro Oggioni, Micha Silver, Luigi Ranghetti & Paolo Tagliolato.
(2026). ReLTER: An Interface for the eLTER Community (v3.1). Zenodo.
<https://doi.org/10.5281/zenodo.5576813>

``` bibtex
@software{alessandro_oggioni_2025_5576813,
  author       = {Alessandro Oggioni and Micha Silver and Luigi Ranghetti and Paolo Tagliolato},
  title        = {ReLTER: An Interface for the eLTER Community},
  year         = 2026,
  publisher    = {Zenodo},
  version      = {v3.1},
  doi          = {10.5281/zenodo.5576813},
  url          = {https://doi.org/10.5281/zenodo.5576813}
}
```

## 📖 Documentation

Visit the [ReLTER](https://docs.ropensci.org/ReLTER) website at
[docs.ropensci.org/ReLTER/](https://docs.ropensci.org/ReLTER/) for
further documentation, examples, and installation of the package.

The manual of [ReLTER](https://docs.ropensci.org/ReLTER) package could
be found [here](https://ropensci.r-universe.dev/manual/ReLTER.pdf).

## 📔 Cheatsheet

[![ReLTER
Cheatsheet](https://raw.githubusercontent.com/ropensci/ReLTER/main/man/figures/ReLTER_cheatSheet_front.png)](https://raw.githubusercontent.com/ropensci/ReLTER/main/man/cheatsheets/ReLTER_cheatSheet.pdf)
[![ReLTER
Cheatsheet](https://raw.githubusercontent.com/ropensci/ReLTER/main/man/figures/ReLTER_cheatSheet_back.png)](https://raw.githubusercontent.com/ropensci/ReLTER/main/man/cheatsheets/ReLTER_cheatSheet.pdf)

## ⏬ Installation

You can install the main version of
[ReLTER](https://docs.ropensci.org/ReLTER) from
[GitHub](https://github.com/ropensci/ReLTER) with:

``` R
install.packages("devtools")
devtools::install_github("ropensci/ReLTER")
library(ReLTER)
```

If you want to install different package branch (e.g. ‘dev’) can you use
this command:

``` R
devtools::install_github('https://github.com/ropensci/ReLTER', ref = 'dev')
```

The [ReLTER](https://docs.ropensci.org/ReLTER) package is part of the
[R-universe](https://r-universe.dev/) community and it can be installed
also use this command:

``` R
utils::install.packages("ReLTER", repos = "https://ropensci.r-universe.dev")
```

Alternatively {`ReLTER`} can be used [in a Docker
container](https://docs.ropensci.org/ReLTER/articles/rocker_ReLTER.md).

If you wish to help develop this package, please follow the
[contributing
guidelines](https://docs.ropensci.org/ReLTER/CONTRIBUTING.md).

## 👩‍💻 Persons involved 👨‍💻

Alessandro Oggioni <https://orcid.org/0000-0002-7997-219X> (CNR,
Institute for Electromagnetic Sensing of the Environment - IREA)

Micha Silver <https://orcid.org/0000-0002-1128-1325> (Ben Gurion
University - BGU)

Luigi Ranghetti <https:://orcid.org/0000-0001-6207-5188> (CNR, Institute
for Electromagnetic Sensing of the Environment - IREA)

Paolo Tagliolato <https:://orcid.org/0000-0002-0261-313X> (CNR,
Institute for Electromagnetic Sensing of the Environment - IREA)

For a exhaustive list of contributors please visit [authors
page](https://docs.ropensci.org/ReLTER/authors).

## 🏢 Contributing organizations

![CNR-IREA](reference/figures/irea_logo.png)

![BGU](reference/figures/bgu_logo.png)

## 👍 Acknowledgements

This work has been partially funded from the European Union’s Horizon
2020 research and innovation programme under the [**H2020 eLTER-Plus
project**](https://elter-ri.eu/elter-plus) Grant Agreement No. 871128
(DOI: [10.3030/871128](https://doi.org/10.3030/871128)) and [**eLTER
EnRich project**](https://elter-ri.eu/elter-enrich) Grant Agreement
No. 101131751 (DOI:
[10.3030/101131751](https://doi.org/10.3030/101131751))

Thanks to the reviewers and the editor ([more about
authors](https://docs.ropensci.org/ReLTER/authors.html)) for their work.
