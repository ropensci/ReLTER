# Obtain the information about of an eLTER location.

**\[stable\]** This function obtains the information about of an eLTER
location (e.g.
<https://deims.org/location/12b38f3f-7e72-425a-80c7-7cad35ce4c7b>)
provided in [DEIMS-SDR catalogue](https://deims.org/).

## Usage

``` r
get_location_info_internal(locationid, show_map = FALSE)
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

The output of the function is a `list` with two elements:

- `map` A Leaflet map with the location, if requested with `show_map`.

- `data` A `data.frame` with the information about the location.

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>

Paolo Tagliolato, phD <tagliolato.p@irea.cnr.it>
