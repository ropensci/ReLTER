# eLTER get_id function

Internal function to retrieve json content from ID

## Usage

``` r
get_id(deimsid, resource = "sites", test = NULL, ...)
```

## Arguments

- deimsid:

  A `character`. It is the DEIMS ID of the site, activity or dataset
  from DEIMS-SDR website. DEIMS ID information
  [here](https://deims.org/docs/deimsid.html).

- resource:

  Character: one among `"sites"` (default), `"activities"` or
  `"datasets"` (`"networks"` currently not tested).

- test:

  Logical: if TRUE, content is not retrieved from <https://deims.org>
  but internally (only for some defined IDs). Used for testing purposes
  to avoid errors in case or HTTP error 500. If missing, the content of
  the internal variable

- ...:

  Arguments to be passed to
  [`httr::RETRY()`](https://httr.r-lib.org/reference/RETRY.html).

## Value

A character containing the json content, and an attribute `status`.

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>

Luigi Ranghetti, phD (2021) <luigi@ranghetti.info>

Paolo Tagliolato, PhD (2021) <tagliolato.p@irea.cnr.it>
