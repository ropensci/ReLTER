# Obtain the information about of an eLTER activity.

**\[stable\]** This function obtains the information about of an eLTER
activity (e.g.
<https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845>)
provided in [DEIMS-SDR catalogue](https://deims.org/).

## Usage

``` r
get_activity_info(activityid, show_map = FALSE)
```

## Arguments

- activityid:

  A `character`. It is the DEIMS ID of activity make from DEIMS-SDR
  website. DEIMS ID information
  [here](https://deims.org/docs/deimsid.html). The DEIMS.iD of activity
  is the URL for the activity page.

- show_map:

  A `boolean`. If TRUE a Leaflet map with occurrences is shown. Default
  FALSE.

## Value

The output of the function is a `list` with two elements:

- `map` A Leaflet map with the activity location, if requested with
  `show_map`.

- `data` A `data.frame` with the information about the activity.

## The function output

![Map of "Study of non-indigenous (alien) species in the Mar Piccolo of
Taranto" activity](figures/get_activity_info_fig.png)

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>

## Examples

``` r
activities <- get_activity_info(
  activityid =
    "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845",
  show_map = TRUE
)
activities
#> $map
#> 
#> $data
#> Simple feature collection with 1 feature and 19 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 17.22656 ymin: 40.46438 xmax: 17.33162 ymax: 40.50408
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 20
#>   title    abstract keywords uri   type  created             changed            
#> * <chr>    <chr>    <list>   <chr> <chr> <dttm>              <dttm>             
#> 1 Study o… "The in… <df>     http… acti… 2021-01-22 10:16:33 2021-01-28 16:06:42
#> # ℹ 13 more variables: relatedSite <list>, dateRange.from <date>,
#> #   dateRange.to <lgl>, contacts.corresponding <list>,
#> #   contacts.metadataProvider <list>, boundaries <POLYGON [°]>,
#> #   availability.digitally <lgl>, availability.forEcopotential <lgl>,
#> #   availability.openData <lgl>, availability.notes <lgl>,
#> #   availability.source <lgl>, observationParameters <lgl>,
#> #   relatedResources <lgl>
#> 
```
