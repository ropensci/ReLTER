# Produce a waffle chart of the observed properties collected in an eLTER site

**\[stable\]** Returns a waffle chart of environmental observed
properties, as stored in [DEIMS-SDR catalogue](https://deims.org/), for
a single eLTER site. The chart is built with ggplot2 and requires no
additional packages. Each square represents one observed property;
squares of the same colour belong to the same parameter group (e.g.
biological, atmospheric, etc.).

## Usage

``` r
produce_site_observedProperties_waffle(deimsid)
```

## Arguments

- deimsid:

  A `character`. The DEIMS ID of the site from DEIMS-SDR website. DEIMS
  ID information [here](https://deims.org/docs/deimsid.html).

## Value

The function prints a waffle chart as a side effect and returns a
`tibble` with four columns:

- `parameterGroups` `character`. Name of the parameter group.

- `n` `integer`. Number of observed properties in the group.

- `freq` `double`. Relative frequency of the group.

- `label` `character`. Percentage label for the group.

Returns `invisible(NULL)` if the DEIMS ID is invalid or the site has no
observed properties.

## The function output

![Observed properties waffle
chart](figures/produce_site_parameters_waffle_fig.png)

## See also

[`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html)

[`get_site_info()`](https://docs.ropensci.org/ReLTER/reference/get_site_info.md)

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
waffle <- produce_site_observedProperties_waffle(
  deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
)
waffle
} # }
```
