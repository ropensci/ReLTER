# Obtain the information about of an eLTER dataset.

**\[deprecated\]** This function obtains the information about of an
eLTER dataset (e.g.
<https://deims.org/dataset/38d604ef-decb-4d67-8ac3-cc843d10d3ef>)
provided in [DEIMS-SDR catalogue](https://deims.org/).

## Usage

``` r
get_dataset_info(datasetid, show_map = FALSE)
```

## Arguments

- datasetid:

  A `character`. It is the DEIMS ID of dataset make from DEIMS-SDR
  website. DEIMS ID information
  [here](https://deims.org/docs/deimsid.html). The DEIMS ID of dataset
  is the URL for the dataset page.

- show_map:

  A `boolean`. If TRUE a Leaflet map with occurrences is shown. Default
  FALSE.

## Value

The output of the function is a `list` with two elements:

- `map` A Leaflet map with the dataset location, if requested with
  `show_map`.

- `data` A `data.frame` with the information about the dataset.

## The function output

![Map of "LTER Northern Adriatic Sea (Italy) marine data from 1965 to
2015" dataset](figures/get_dataset_info_fig.png)

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
tDataset <- get_dataset_info(
  datasetid =
  "https://deims.org/dataset/38d604ef-decb-4d67-8ac3-cc843d10d3ef",
  show_map = TRUE
)
tDataset
} # }
```
