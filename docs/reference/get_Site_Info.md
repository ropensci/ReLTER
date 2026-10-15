# Obtain details about an eLTER site.

**\[stable\]** This function obtains information of a single eLTER site,
as a stored in [DEIMS-SDR catalogue](https://deims.org/), through the
DEIMS-SDR API.

## Usage

``` r
get_site_info(
  deimsid,
  categories = NA,
  show_map = FALSE,
  with_locations = FALSE
)
```

## Arguments

- deimsid:

  A `character`. The DEIMS ID of the site from DEIMS-SDR website. DEIMS
  ID information [here](https://deims.org/docs/deimsid.html).

- categories:

  A `categories`. This parameter selects which categories or categories
  are retrieved and returned in the result. Possible value are:
  'Affiliations', 'Contacts', 'EnvCharacts', 'General',
  'Infrastructure', 'observedProperties', 'RelateRes'. Multiple values
  can be indicated. A site's boundary is always returned.

- show_map:

  A `boolean`. When TRUE a `leaflet` map is plotted as side effect.
  Default FALSE.

- with_locations:

  A `boolean`. When TRUE, and only `show_map` is TRUE, all site related
  locations are showed in the plotted map. Default FALSE.

## Value

The output of the function is a `sf` with the information about the
site. If the boundary is missing from DEIMS-SDR a `tibble` is returned.

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>

Paolo Tagliolato, phD <tagliolato.p@irea.cnr.it>

## Examples

``` r
site <- get_site_info(
  deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
  categories = c("EnvCharacts", "Affiliations"),
  show_map = TRUE,
  with_locations = FALSE
)
site
#> Simple feature collection with 1 feature and 32 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 8.47803 ymin: 45.72556 xmax: 8.860755 ymax: 46.18081
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 33
#>   title.x         uri   created             changed             geoCoord country
#>   <chr>           <chr> <dttm>              <dttm>              <chr>    <chr>  
#> 1 Lago Maggiore … http… 2012-12-25 17:57:05 2025-02-11 08:30:43 POINT (… Italy  
#> # ℹ 27 more variables: geoElev.avg [m], geoElev.min [m], geoElev.max [m],
#> #   geoElev.unit <chr>, lterSiteClassification <lgl>, networks <list>,
#> #   projects <list>, airTemperature.yearlyAverage [°C],
#> #   airTemperature.monthlyAverage <list>, airTemperature.unit <chr>,
#> #   airTemperature.referencePeriod [°C], precipitation.yearlyAverage [mm],
#> #   precipitation.monthlyAverage <list>, precipitation.unit <chr>,
#> #   precipitation.referencePeriod [mm], biogeographicalRegion <chr>, …
```
