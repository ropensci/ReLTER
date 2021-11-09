
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ReLTER <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->
<!-- other badges https://github.com/GuangchuangYu/badger -->
<!-- DOI badge -->

[![](https://img.shields.io/badge/doi-10.5281/zenodo.5576813-yellow.svg)](https://doi.org/10.5281/zenodo.5576813)
<!-- the version is determined via the DESCRIPTION file -->
[![](https://img.shields.io/badge/devel%20version-0.2.0-blue.svg)](https://github.com/oggioniale/ReLTER)
<!-- CRAN badges -->
<!-- [![](https://www.r-pkg.org/badges/version/oggioniale/ReLTER?color=orange)](https://cran.r-project.org/package=oggioniale/ReLTER) -->
<!-- [![CRAN checks](https://cranchecks.info/badges/summary/ReLTER)](https://cran.r-project.org/web/checks/check_results_ReLTER.html) -->
[![](https://img.shields.io/github/languages/code-size/oggioniale/ReLTER.svg)](https://github.com/oggioniale/ReLTER)
[![](https://img.shields.io/github/last-commit/oggioniale/ReLTER.svg)](https://github.com/oggioniale/ReLTER/commits/main)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- Once you have submitted a package and it has passed editor checks, add a peer-review badge via -->
<!-- [![](https://badges.ropensci.org/<issue_id>_status.svg)](https://github.com/ropensci/software-review/issues/<issue_id>) -->
<!-- where issue_id is the number of the issue in the software-review repository. -->
<!-- badges: end -->

`{ReLTER}` is an R package that: provides access to
[DEIMS-SDR](https://deims.org/)), allows interact with software
implemented by eLTER Research Infrastructure (RI) and improves the
data/information shared by them. `{ReLTER}` is a R package devoted to
access, interact and improve the information and the data shared by Long
Term Ecological Research (LTER) network. This package is born within
eLTER H2020 major project that will help advance the development of
European Long-Term Ecosystem Research Infrastructures ([eLTER
RI](https://elter-ri.eu)).

The `{ReLTER}` package functions in particular allow to:

-   retrive the information about entities (e.g. sites, datasets, and
    activities) shared by [DEIMS-SDR](https://deims.org/) (see
    e.g. [get_site_info
    function](https://oggioniale.github.io/ReLTER/reference/get_site_info.html));

-   elaborate the information of single site or merge info from national
    network sites or entire International LTER (ILTER) in order to
    provide maps, figures, graphs etc (see
    e.g. [get_network_sites](https://oggioniale.github.io/ReLTER/reference/get_network_sites.html),
    [produce_site_map](https://oggioniale.github.io/ReLTER/reference/produce_site_map.html)
    or
    [produce_site_parameters_pie](https://oggioniale.github.io/ReLTER/reference/produce_site_parameters_pie.html)
    functions);

-   use the site informations for download data from other platforms
    (see
    e.g. [get_site_ODS](https://oggioniale.github.io/ReLTER/reference/get_site_ODS.html)
    function);

-   improve the quality of the dataset (see
    e.g. [get_id_worms](https://oggioniale.github.io/ReLTER/reference/get_id_worms.html)).

Functions currently implemented are derived from the discussion of the
needs declared by eLTER users community.

The `{ReLTER}` package shall definitely follow the progress of eLTER-RI
infrastructure and evolving with the improvements and develop of new
tools.

<!-- about the icons https://github.com/ikatyang/emoji-cheat-sheet -->

## :notebook_with_decorative_cover: Citation

To cite `{ReLTER}` please use: …

or:

``` bibtex
@software{
...
}
```

## :book: Documentation

You can visit `{ReLTER}` website at
[oggioniale.github.io/ReLTER/](https://oggioniale.github.io/ReLTER/) for
obtain documentation on installation, functions description and examples
of use.

## :arrow_double_down: Installation

You can install the development version of `{ReLTER}` from
[GitHub](https://github.com/oggioniale/ReLTER) with:

``` r
install.packages("devtools")
devtools::install_github("oggioniale/ReLTER")
```

## :memo: Examples

Some examples of the possible capabilities of this library is given
below. In these examples you can see the interaction, througth
[API](https://deims.org/api), with [DEIMS-SDR](https://deims.org/).

The *get_site_info* function, with category Boundaries, creates a map
overlaying the boundaries of the site (e.g. Lake Maggiore) thanks to the
information on geographical aspects provided by
[DEIMS-SDR](https://deims.org/).

``` r
library(dplyr)
siteBoundaries <- ReLTER::get_site_info(
  deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe',
  category = "Boundaries"
)
#>  Found 1 records... Imported 1 records. Simplifying...
#>  Found 1 records... Imported 1 records. Simplifying...
leaflet::leaflet(siteBoundaries) %>% 
  leaflet::addTiles() %>% 
  leaflet::addPolygons()
```

<img src="man/figures/README-exampleSiteBoundaries-1.png" width="100%" />

``` r
siteBoundaries
#> Simple feature collection with 1 feature and 8 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 8.47803 ymin: 45.72556 xmax: 8.860755 ymax: 46.18081
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 9
#>   title uri                  boundaries geoCoord country geoElev.avg geoElev.min
#>   <chr> <chr>        <MULTIPOLYGON [°]> <chr>    <list>        <dbl>       <int>
#> 1 Lago… http… (((8.615976 45.72628, 8.… POINT (… <chr […        194.         186
#> # … with 2 more variables: geoElev.max <int>, geoElev.unit <chr>
```

------------------------------------------------------------------------

The *get_network_parameters* function creates a list of parameters
collected by a network (e.g. LTER-Italy). Information always gathered
from what is indicated in the different sites on
[DEIMS-SDR](https://deims.org/).

| parameterLabel                    | parameterUri                                  |
|:----------------------------------|:----------------------------------------------|
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

------------------------------------------------------------------------

The *get_dataset_info* function provides to a table with information
about specific dataset shared through [DEIMS-SDR](https://deims.org/).

    #>  Found 1 records... Imported 1 records. Simplifying...

<img src="man/figures/README-exampleGetDataset-1.png" width="100%" />

    #> Simple feature collection with 1 feature and 33 fields
    #> Geometry type: POLYGON
    #> Dimension:     XY
    #> Bounding box:  xmin: 11.88721 ymin: 43.20518 xmax: 15.86426 ymax: 45.91294
    #> Geodetic CRS:  WGS 84
    #> # A tibble: 1 × 34
    #>   title  abstract   keywords uri   type  dateRange.from dateRange.to relatedSite
    #> * <chr>  <chr>      <list>   <chr> <chr> <chr>          <lgl>        <list>     
    #> 1 LTER … The prese… <df [5 … http… data… 1965-01-01     NA           <df [1 × 4…
    #> # … with 26 more variables: contacts.corresponding <list>,
    #> #   contacts.creator <list>, contacts.metadataProvider <lgl>,
    #> #   observationParameters <list>, observationSpecies <list>, dataPolicy <list>,
    #> #   doi <chr>, onlineLocation <list>, legal.accessUse <list>,
    #> #   legal.rights <lgl>, legal.legalAct <lgl>, legal.citation <lgl>,
    #> #   method.instrumentation <lgl>, method.qualityAssurance <lgl>,
    #> #   method.methodUrl <list>, method.methodDescription <list>, …

------------------------------------------------------------------------

The *get_site_info* function, with category = “RelateRes”, provides a
list of related resources associated with a site within
[DEIMS-SDR](https://deims.org/).

    #>  Found 1 records... Imported 1 records. Simplifying...
    #>  Found 1 records... Imported 1 records. Simplifying...
    #>                                                                              relatedResourcesTitle
    #> 1 Biovolume of Phytoplankton in Lake Maggiore site code  IT_SI001137_within the period 1981 - 2010
    #> 2                           Atmospheric deposition in Pallanza, Lake Maggiore watershed, 1980-2018
    #> 3                                     Phytoplankton_Biomass_Lake Maggiore_Ghiffa_station-1984-2018
    #> 4                                      Water chemistry of Lake Maggiore, Ghiffa station, 1988-2018
    #> 5                          Transparency (Secchi depth) of Lake Maggiore, Ghiffa station, 1988-2018
    #> 6                                             Chlorophyll a_Lake_Maggiore_Ghiffa_Station-1984-2018
    #> 7                                 Water discharge of River Ticino, Lake Maggiore outlet, 1988-2018
    #>    relatedResourcesChanged
    #> 1 2021-08-25T16:38:25+0200
    #> 2 2020-12-13T20:06:48+0100
    #> 3 2020-12-16T10:46:15+0100
    #> 4 2021-11-03T06:10:14+0100
    #> 5 2020-12-13T20:10:34+0100
    #> 6 2021-01-10T21:48:49+0100
    #> 7 2021-07-21T12:35:07+0200
    #>                                                              uri
    #> 1 https://deims.org/dataset/d9e94776-e7a8-11e2-a655-005056ab003f
    #> 2 https://deims.org/dataset/0ce46362-0aab-482a-b1f0-a444a5dada39
    #> 3 https://deims.org/dataset/0ab8425a-d574-4575-8ba9-5275c607b0c5
    #> 4 https://deims.org/dataset/69564188-89de-4879-ad88-4aa97c1d005d
    #> 5 https://deims.org/dataset/e538c743-2149-49e3-9025-14a04ea7c90d
    #> 6 https://deims.org/dataset/c857c8e2-48aa-4dcd-a7fb-e089bd4c5c4e
    #> 7 https://deims.org/dataset/fb3a8fec-0c1f-4c3a-81d5-364c7e6078c4

------------------------------------------------------------------------

The *produce_site_parameters_waffle* function provides a grouping of
parameters, declared as measured within a site, in a waffle chart
representation.

``` r
ReLTER::produce_site_parameters_waffle(
  deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
)
#>  Found 1 records... Imported 1 records. Simplifying...
#>  Found 1 records... Imported 1 records. Simplifying...
```

<img src="man/figures/README-exampleproduce_site_parameters_waffle-1.png" width="100%" />

    #> # A tibble: 11 × 4
    #>    parameterGroups              n   freq label
    #>    <chr>                    <int>  <dbl> <chr>
    #>  1 agricultural parameter       1 0.0105 1.1% 
    #>  2 atmospheric parameter       14 0.147  14.7%
    #>  3 biological parameter         8 0.0842 8.4% 
    #>  4 chemical parameter          24 0.253  25.3%
    #>  5 ecosystem parameter         23 0.242  24.2%
    #>  6 genetic parameter            2 0.0211 2.1% 
    #>  7 landscape parameter          4 0.0421 4.2% 
    #>  8 physical parameter           1 0.0105 1.1% 
    #>  9 remote sensing parameter     1 0.0105 1.1% 
    #> 10 soil parameter               1 0.0105 1.1% 
    #> 11 water parameter             16 0.168  16.8%

## :woman_technologist: Persons involved :man_technologist:

Alessandro Oggioni <https://orcid.org/0000-0002-7997-219X> (CNR,
Institute for Electromagnetic Sensing of the Environment - IREA)

<!-- Add authors, reviewers including those who, within eLTER network, reviewed the package and add acknowledgements 
     in accordance with this https://ropensci.org/blog/2018/03/16/thanking-reviewers-in-metadata/ and this
     https://devguide.ropensci.org/building.html#authorship.
     Add the same in DESCRIPTION file -->

Micha Silver <https://orcid.org/0000-0002-1128-1325> (Ben Gurion
University - BGU)

Paolo Tagliolato <https:://orcid.org/0000-0002-0261-313X> (CNR,
Institute for Electromagnetic Sensing of the Environment - IREA)

For a exhaustive list of contributors please visit [authors
page](https://oggioniale.github.io/ReLTER/authors).

To contribute to the improvement of this package, join the group of
developers in [GitHub](https://github.com/oggioniale/ReLTER).

## :office: Contributing organizations

<img src="man/figures/irea_logo.png" height="70" alt="CNR-IREA" />
<!--a href="http://www.irea.cnr.it/en/"><img src="man/figures/irea_logo.png" height="40" align="left" /></a-->

<img src="man/figures/bgu_logo.png" height="80" alt="BGU" />
<!--a href="https://in.bgu.ac.il/en/"><img src="man/figures/bgu_logo.png" height="40" align="left" /></a-->

## :passport_control: Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.
