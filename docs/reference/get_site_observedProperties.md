# eLTER get_site_observedProperties function

**\[stable\]** This internal function obtains the list of observed
properties measured in the eLTER site through the DEIMS-SDR sites API.

## Usage

``` r
get_site_observedProperties(deimsid)
```

## Arguments

- deimsid:

  A `character`. It is the DEIMS ID of the site from DEIMS-SDR website.
  DEIMS ID information [here](https://deims.org/docs/deimsid.html).

## Value

The output of the function is a `tibble` with main features of the site
and the observed properties collected.

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
