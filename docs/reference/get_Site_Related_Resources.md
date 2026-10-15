# eLTER get_site_related_resources function

**\[stable\]** This internal function obtains a list of related
resources collected in an eLTER site through the DEIMS-SDR sites API.

## Usage

``` r
get_site_related_resources(deimsid)
```

## Arguments

- deimsid:

  A `character`. The DEIMS ID of the site from DEIMS-SDR website. DEIMS
  ID information [here](https://deims.org/docs/deimsid.html).

## Value

The output of the function is a `tibble` with main features of the site
and a list of the related resources collected by site.

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
