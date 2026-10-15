# eLTER get_site_infrastructure function

**\[stable\]** This internal function obtains infrastructure information
of an eLTER site through the DEIMS-SDR sites API.

## Usage

``` r
get_site_infrastructure(deimsid)
```

## Arguments

- deimsid:

  A `character`. The DEIMS ID of the site from DEIMS-SDR website. DEIMS
  ID information [here](https://deims.org/docs/deimsid.html).

## Value

The output of the function is a `tibble` with main features of the site
and infrastructure information where available, such as: power supply,
accessibility, maintenaince interval, etc.

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
