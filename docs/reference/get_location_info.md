# Obtain the information about of an eLTER location.

**\[stable\]** This function obtains the information about of an eLTER
location (e.g.
<https://deims.org/location/12b38f3f-7e72-425a-80c7-7cad35ce4c7b>)
provided in [DEIMS-SDR catalogue](https://deims.org/).

## Usage

``` r
get_location_info(locationid, show_map = FALSE)
```

## Arguments

- locationid:

  A `character`. It is the DEIMS ID of location make from DEIMS-SDR
  website. DEIMS ID information
  [here](https://deims.org/docs/deimsid.html). The DEIMS.iD of activity
  is the URL for the location page.

- show_map:

  A `boolean`. If TRUE a Leaflet map with occurrences is shown. Default
  FALSE.

## Value

The output of the function is a `tibble` with the information about the
location.

## The function output

![Map of "LTER Zöbelboden, Austria, Project area"
location](figures/get_location_info_fig.png)

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>

Paolo Tagliolato, phD <tagliolato.p@irea.cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
# Sampling location multipolygon
location <- get_location_info(
  locationid =
    "https://deims.org/location/85dc6019-9654-4ba0-8338-08c4ffe8fe47",
  show_map = TRUE
)
location

# Sampling location polygon
location <- get_location_info(
  locationid =
    "https://deims.org/location/12b38f3f-7e72-425a-80c7-7cad35ce4c7b",
  show_map = TRUE
)
location

# Equipment location polygon
location <- get_location_info(
  locationid =
    "https://deims.org/locations/04de8301-b481-4ed2-89ff-2f48562e2514",
  show_map = TRUE
)
location

# Sampling location point
location <- get_location_info(
  locationid =
    "https://deims.org/location/ec1a58f7-1aee-4e3f-bec3-4eb1516ee905",
  show_map = TRUE
)
location

# Sampling location point with location type null
location <- get_location_info(
  locationid =
    "https://deims.org/location/c3db70c3-5d2c-4905-801c-7b7a5c4d00d9",
  show_map = TRUE
)
location
} # }
```
