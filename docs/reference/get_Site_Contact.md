# eLTER get_site_contact function

**\[stable\]** This internal function obtains the contact information
for an eLTER site through the DEIMS-SDR sites API.

## Usage

``` r
get_site_contact(deimsid)
```

## Arguments

- deimsid:

  A `character`. It is the DEIMS ID of the site from DEIMS-SDR website.
  DEIMS ID information [here](https://deims.org/docs/deimsid.html).

## Value

The output of the function is a `tibble` with main features of the site
and the contact information, such as: site manager, operation
organization, metadata provider, founding agency and site url.

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
