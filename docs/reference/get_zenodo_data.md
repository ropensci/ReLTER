# Obtain the data from a dataset deposited in Zenodo record.

**\[deprecated\]** The function download the file(s) deposited in Zenodo
record and returns a tibble with metadata.

## Usage

``` r
get_zenodo_data(doi, rdata_exist = TRUE)
```

## Arguments

- doi:

  A `character`. It is the DOI of the Zenodo record.

- rdata_exist:

  A `logical`. Is the .RData or .rds file in the record we are
  questioning? Default TRUE.

## Value

a file(s) containing in the Zenodo record and tibble contains some
metadata of Zenodo record.

## See also

[`zen4R::ZenodoManager()`](https://rdrr.io/pkg/zen4R/man/ZenodoManager.html)

[`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html)

[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html)

## Author

Alessandro Oggioni, phD <oggioni.a@irea.cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
## Not run:

record <- get_zenodo_data(
  doi = "10.5281/zenodo.7041152", # test dataset
  rdata_exist = TRUE
)
record

## End (Not run)
} # }
```
