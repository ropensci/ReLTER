# Select sites by choosing a 3d bounding box through an interactive GUI.

**\[experimental\]** Open a GUI (a shiny widget) to interact with the
user for the selection of a 3D bounding box. The user can visualise the
sites within the selected bounding box and ask for returning the
contained sites or simply the bounding box.

## Usage

``` r
get_sites_interactive()
```

## Value

`tibble` with selected sites or a `list` with selected bounding box
(with slots `bbx` and `elevation_range`).

## See also

[shiny::runGadget](https://rdrr.io/pkg/shiny/man/runGadget.html)

[`leaflet.extras::addDrawToolbar()`](https://rdrr.io/pkg/leaflet.extras/man/draw.html)

## Author

Paolo Tagliolato

## Examples

``` r
if (FALSE) { # \dontrun{
sites_tbl_sf <- get_sites_interactive()
} # }
```
