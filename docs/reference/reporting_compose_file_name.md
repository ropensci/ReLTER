# eLTER reporting format naming convention for files

**\[experimental\]** Compose file name following eLTER naming convention

## Usage

``` r
reporting_compose_file_name(
  deimsid = NULL,
  data_topic,
  variable_group = "",
  time_span,
  version = Sys.Date() %>% format("V%Y%m%d")
)
```

## Arguments

- deimsid:

  A `character` The DEIMS ID of the site from DEIMS-SDR website. More
  information about DEIMS ID in this pages:
  [page](https://deims.org/docs/deimsid.html).

- data_topic:

  A `character`. Max 5-digit code for data topic or observation
  programme, e.g. METEO (Meteorology), BIODIV (Biodiversity), DEPO
  (deposition), GHG (Green House gas), SW (Soil water), VEG
  (Vegetation). The abbreviation is defined by the data provider
  depending on the data.

- variable_group:

  A `character`. Optional, list of variables or variable groups
  contained in the data. The abbreviation is defined by the data
  provider depending on the data.

- time_span:

  A `numeric` or a `character`. Time span covered in the data. E.g.
  2015, 20150302-20180415. The time span is defined by the data
  provider.

- version:

  version in format "VYYYYMMDD". Data version in the format “V”YYYYMMDD.
  Defaults to current system date.

## Value

filename (without extension) following naming convention

## Note

This method must be intended as a signpost for future implementation

## See also

Peterseil, Geiger et al. (2020) Field Specification for data reporting.
Technical Document. TechDoc.01. EU Horizon 2020 eLTER PLUS Project,
Grant agreement No. 871128 <https://zenodo.org/record/6373410>

## Author

Paolo Tagliolato, phD <tagliolato.p@irea.cnr.it>

Alessandro Oggioni, phD <oggioni.a@irea.cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
## Not run:

deimsid <- "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6"
time_span <- 2015 # e.g. whole year
# time_span <- "20150302-20180415" # e.g. span between two dates
data_topic <- "VEG" # data provider defined abbreviation of "vegetation"
variable_group <- "SPECCOVER" # data provider defined abbreviation
version <- "V20220907"

filename <- reporting_compose_file_name(
  deimsid = deimsid,
  data_topic = data_topic,
  variable_group = variable_group,
  time_span = time_span,
  version = version
)

## End (Not run)
} # }
```
