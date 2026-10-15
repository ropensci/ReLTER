# Create location map

This is an internal function for create only the map of location.

## Usage

``` r
map_add_location(location_data, map = NULL)
```

## Arguments

- location_data:

  A `sf` object. It is the location geodata provided by
  [`get_location_info()`](https://docs.ropensci.org/ReLTER/reference/get_location_info.md).

- map:

  A `leaflet` object. It is an empty map by default.

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>

Paolo Tagliolato, phD (2020) <tagliolato.p@irea.cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
## Not run:
map <- map_add_location(
  location_data = location$data,
  map = NULL
)
} # }
## End (Not run)
```
