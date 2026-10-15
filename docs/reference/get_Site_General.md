# eLTER get_site_general function

**\[stable\]** This internal function obtains general information about
an eLTER site through the DEIMS-SDR sites API.

## Usage

``` r
get_site_general(deimsid)
```

## Arguments

- deimsid:

  A `character`. The DEIMS ID of the site from DEIMS-SDR website. DEIMS
  ID information [here](https://deims.org/docs/deimsid.html).

## Value

The output of the function is a `tibble` with main features of the site
and the general information, such as: abstract, purpose, status,
yearEstablished, yearClosed, hierarchy, siteName, short name, site type,
protection level, images.

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
