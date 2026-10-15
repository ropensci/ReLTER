# Harmonize outputs of get_site_speciesOccurrence and map them into eLTER reporting format

**\[experimental\]**

## Usage

``` r
map_occ_gbif2elter(x, deimsid, version = "1.3")
```

## Arguments

- x:

  A `tibble` like one that can be obtained by
  as_tibble(get_site_speciesOccurrences(deimsid, "gbif")\$gbif)

- deimsid:

  A `character`. The DEIMS.iD of the site from DEIMS-SDR website. DEIMS
  ID information

- version:

  A `character` for select the version of eLTER Data Reporting Format.
  Default 1.3

## Value

list with the following named elements:

- deimsid: the same deimsid passed in input

- source: one of "gbif", "inat", "obis"

- data_mapping: tibble structured according to data_mapping of eLTER
  reporting format

- reference_TAXA: tibble structured according to reference_TAXA of eLTER
  reporting format

- reference_VARIABLES: tibble structured according to
  reference_VARIABLES of eLTER reporting format

## Author

Paolo Tagliolato, phD <tagliolato.p@irea.cnr.it>

Martina Zilioli <zilioli.m@irea.cnr.it>

Alessandro Oggioni, phD <oggioni.a@irea.cnr.it>
