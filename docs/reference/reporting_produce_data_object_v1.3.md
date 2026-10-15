# Compose an eLTER Data Reporting Format object

**\[experimental\]** Given several tables, creates an eLTER data
reporting format object

## Usage

``` r
reporting_produce_data_object_v1.3(
  data = NULL,
  station = NULL,
  method = NULL,
  reference = NULL,
  event = NULL,
  sample = NULL,
  license = "",
  deimsid = "",
  data_type = "measurement",
  filename = NULL
)
```

## Arguments

- data:

  A `tibble`. See eLTER data specification format for details

- station:

  A `tibble`

- method:

  A `tibble`

- reference:

  A `tibble`

- event:

  A `tibble`

- sample:

  A `tibble`

- license:

  A `character`

- deimsid:

  A character. The DEIMS ID of the site from DEIMS-SDR website. DEIMS ID
  information [here](https://deims.org/docs/deimsid.html).

- data_type:

  A `character`. Data must be provided by one of measurement or mapping.
  Default 'measurement'

- filename:

  optional filename associated with the object, of the form provided as
  output by the function `reporting_compose_file_name`

## Value

list with eLTER reporting format slots

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

data <- tibble::tribble(
  ~SITE_CODE,
  ~VARIABLE,
  ~TIME,
  ~VALUE,
  ~UNIT,
  "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6", "TEMP", "2016-03-15", "5.5",  "\u00b0C",
  "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6", "PREC", "2016-03-03", "10.2", "mm",
  "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6", "TEMP", "2016-02-15", "2.5",  "\u00b0C",
  "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6", "NH4N", "2016-03",    "5.5",  "mg/l",
  "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6", "SO4S", "2016-03",    "10.2", "mg/l",
  "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6", "CA",   "2016-03",    "2.5",  "mg/l"
)

station <- dplyr::tribble(
  ~SITE_CODE, ~STATION_CODE, ~STYPE, ~LAT,      ~LON,       ~ALTITUDE,
  deimsid,    "IP2",         "AREA",  45.340805, 7.88887495, 265
)
method <- dplyr::tribble(
  ~VARIABLE, ~METH_DESCR,
  "COVE_F",  "Analysis of ammonium..." 
)

research_object <- reporting_produce_data_object_v1.3(
 filename = filename,
 deimsid = deimsid,
 data = data,
 station = station,
 method = method
)

} # }
## End (Not run)
```
