# eLTER get_site_affiliations function

**\[stable\]** This internal function obtains details about an eLTER
site through the DEIMS-SDR sites API.

## Usage

``` r
get_site_affiliations(deimsid)
```

## Arguments

- deimsid:

  A character. The DEIMS ID of the site from DEIMS-SDR website. DEIMS ID
  information [here](https://deims.org/docs/deimsid.html).

## Value

The output of the function is a `tibble` with main features of the site
and the affiliations information, such as: networks and projects in
which the site is involved.

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
