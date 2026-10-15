# select sites within a 3d bounding box

**\[experimental\]** select sites within a 3d bounding box

## Usage

``` r
get_sites_within_3d_bounding_box(bbox, elevation_range = NULL, show_map = TRUE)
```

## Arguments

- bbox:

  bounding box

- elevation_range:

  elevation range

- show_map:

  show the map

## See also

[`geojsonsf::geojson_sf()`](https://rdrr.io/pkg/geojsonsf/man/geojson_sf.html)

## Examples

``` r
if (FALSE) { # \dontrun{
bbx3d<-list(bbx=data.frame(x=c(6.718290, 9.805761), 
                y=c(44.79938, 47.11089)), 
            elevation_range=c(1200, 9000))
get_sites_within_3d_bounding_box(bbx3d)
} # }
```
