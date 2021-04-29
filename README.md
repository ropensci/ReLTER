
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ReLTER <!--img src='man/figures/logo.png' align="right" height="139" /-->

<!-- badges: start -->

<!-- other badges https://github.com/GuangchuangYu/badger -->

[![](https://img.shields.io/badge/devel%20version-0.1.1-blue.svg)](https://github.com/oggioniale/ReLTER)
[![](https://img.shields.io/github/languages/code-size/oggioniale/ReLTER.svg)](https://github.com/oggioniale/ReLTER)
[![](https://img.shields.io/github/last-commit/oggioniale/ReLTER.svg)](https://github.com/oggioniale/ReLTER/commits/main)
<!-- badges: end -->

`{ReLTER}` is an R package that allows interact with software
(e.g. [DEIMS-SDR](https://deims.org/)) implemented by eLTER Research
Infrastructure (RI) and manage the data/information shared by them.

<!-- ## Citation -->

<!-- To cite `{ReLTER}` please use: -->

<!-- ... -->

## :book: Documentation

You can visit `{ReLTER}` website at
[oggioniale.github.io/ReLTER/](https://oggioniale.github.io/ReLTER/) for
obtain more information, documentation and examples of use.

## :arrow\_double\_down: Installation

You can install the development version of `{ReLTER}` from
[GitHub](https://github.com/oggioniale/ReLTER) with:

``` r
install.packages("devtools")
devtools::install_github("oggioniale/ReLTER")
```

## Examples

Some examples of the possible capabilities of this library is given
below. In these examples you can see the interaction, througth
[API](https://deims.org/api), with [DEIMS-SDR](https://deims.org/).

The *getSiteBoundaries* function creates a map overlaying the boundaries
of the site (e.g. Lake Maggiore) thanks to the information on
geographical aspects provided by [DEIMS-SDR](https://deims.org/).

``` r
library(dplyr)
ReLTER::getSiteBoundaries(deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe')
#> Deleting layer `sites_Lago_Maggiore_-_Italy' using driver `ESRI Shapefile'
#> Writing layer `sites_Lago_Maggiore_-_Italy' to data source `sites_Lago_Maggiore_-_Italy.shp' using driver `ESRI Shapefile'
#> Writing 1 features with 2 fields and geometry type Multi Polygon.
```

<img src="man/figures/README-exampleSiteBoundaries-1.png" width="100%" />

-----

The *getNetworkParameters* function creates a list of parameters
collected by a network (LTER-Italy) information always gathered from
what is indicated in the different sites on DEIMS-SDR.

``` r
library(dplyr)
listParams <- ReLTER::getNetworkParameters(networkDEIMSID = 'https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3')
knitr::kable(listParams[1:10, ] %>% dplyr::rows_insert(tibble(parameterLabel = "...", parameterUri = "...")))
```

| parameterLabel                    | parameterUri                                  |
| :-------------------------------- | :-------------------------------------------- |
| atmospheric parameter             | <http://vocabs.lter-europe.net/EnvThes/20937> |
| ecosystem parameter               | <http://vocabs.lter-europe.net/EnvThes/20939> |
| conductivity                      | <http://vocabs.lter-europe.net/EnvThes/22089> |
| dissolved nutrient                | <http://vocabs.lter-europe.net/EnvThes/22107> |
| dissolved organic carbon in water | <http://vocabs.lter-europe.net/EnvThes/10307> |
| ecosystem structure               | <http://vocabs.lter-europe.net/EnvThes/21475> |
| inorganic nutrient content        | <http://vocabs.lter-europe.net/EnvThes/22137> |
| percent carbon                    | <http://vocabs.lter-europe.net/EnvThes/22181> |
| percent organic carbon            | <http://vocabs.lter-europe.net/EnvThes/22183> |
| total carbon                      | <http://vocabs.lter-europe.net/EnvThes/22296> |
| …                                 | …                                             |

## :writing\_hand: Authors

Alessandro Oggioni <http://www.cnr.it/people/alessandro.oggioni>

Paolo Tagliolato \<…\>

CNR - Institute for Electromagnetic Sensing of the Environment (IREA)

## Contributing organizations

<a href="http://www.irea.cnr.it/en/"><img src="man/figures/irea_logo.png" height="40" align="left" /></a>
