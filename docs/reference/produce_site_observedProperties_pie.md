# Produce a pie chart of the observed properties collected in a site LTER.

**\[stable\]** Return a pie chart of Environmental observed properties,
as a stored in [DEIMS-SDR catalogue](https://deims.org/), of a single
eLTER site.

## Usage

``` r
produce_site_observedProperties_pie(deimsid)
```

## Arguments

- deimsid:

  A `character`. It is the DEIMS ID of site/network from DEIMS-SDR
  website. DEIMS ID information
  [here](https://deims.org/docs/deimsid.html).

## Value

The output of the function is a pie chart and a `tibble`. The
percentages, as a label in the pie charts and in the output table (
column 'perc'), refer to the number of the observed properties,
belonging to a type (e.g. biological, atmospheric, etc.), measured
compared to all of observed properties measured into selected eLTER
site. This function allows to show what type of observed properties are
most measured into a site. In the example below the atmospheric observed
properties corresponds to the 15 percent of all observed properties
measured into the site.

## The function output

![Observed properties pie
chart](figures/produce_site_parameters_pie_fig.png)

## See also

[`ggforce::geom_arc_bar()`](https://ggforce.data-imaginist.com/reference/geom_arc_bar.html)

[`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html)

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
pie <- produce_site_observedProperties_pie(
  deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
)
} # }
```
