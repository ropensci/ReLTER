# eLTER get_site_boundaries function

**\[stable\]** This internal function retrieves the boundary of a
specified eLTER site and a view of the site boundaries on a leaflet map
is shown. All the info are taken from the DEIMS-SDR sites API. If the
boundary is missing, a warning message is printed in the R console.

## Usage

``` r
get_site_boundaries(deimsid, show_map = FALSE, with_locations = FALSE)
```

## Arguments

- deimsid:

  A `character`. The DEIMS ID of the site from DEIMS-SDR website. DEIMS
  ID information [here](https://deims.org/docs/deimsid.html).

- show_map:

  A `boolean` or the string "return". When TRUE a `leflet` object (a
  map) is returned and plotted. Default FALSE. When the value is
  "return", the `leaflet` object is returned but not plotted.

- with_locations:

  A `boolean`. When TRUE all site boundaries and related locations are
  returned (Sampling Location or Equipment Location). Default FALSE.

## Value

The output of the function is a `list` with slots:

- `data` An `sf` object with the the boundary of the site. If the
  boundary is missing from DEIMS-SDR, a `tibble` with the name and URI
  of the site.

- `locations` A `tibble` with the locations' details if `with_locations`
  is TRUE, otherwise NULL.

## The function output

![Map of "LTER Zöbelboden, Austria" with
locations](figures/get_site_boundaries_fig.png)

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>

Micha Silver, phD (2021) <silverm@post.bgu.ac.il>

Paolo Tagliolato, phD <tagliolato.p@irea.cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
# LTER Zöbelboden
boundaries <- get_site_boundaries(
  deimsid = "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",
  show_map = TRUE,
  with_locations = TRUE
)
boundaries
} # }
```
