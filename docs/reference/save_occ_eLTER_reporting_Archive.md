# Creates an archive with files following the eLTER reportingFormat

**\[experimental\]** Creates a zip archive named
biodiv_occurrence_site\_"deimsid_code"\_"source".zip where
"deimsid_code" is the uuid in the last part of the deimsid, and "source"
is one of "gbif", "inat", "obis"

## Usage

``` r
save_occ_eLTER_reporting_Archive(lterReportOut, path = tempdir())
```

## Arguments

- lterReportOut:

  A `list` like the one created by `map_occ_gbif2elter`

- path:

  path of the zip file. Defaults to temporary folder

## Value

the path to the created file

## Author

Paolo Tagliolato, phD <tagliolato.p@irea.cnr.it>
