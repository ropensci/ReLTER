# eLTER get_site_envcharacts function

**\[stable\]** This internal function obtains Environmental
Characteristics of an eLTER site through the DEIMS-SDR sites API.

## Usage

``` r
get_site_envcharacts(deimsid)
```

## Arguments

- deimsid:

  A `character`. The DEIMS ID of a site from DEIMS-SDR website. DEIMS ID
  information [here](https://deims.org/docs/deimsid.html).

## Value

The output of the function is a `tibble` with main features of the site
and the environmental characteristics where available, such as: air
temperature, precipitation, biogeographical region, biome, ecosystem
land use, EUNIS habitat, geoBon biome, geology, hydrology, soils and
vegetation.

## Author

Alessandro Oggioni, phD (2021) <oggioni.a@irea.cnr.it>
