# Creates an archive with files following the eLTER reportingFormat

**\[experimental\]** Creates a zip archive "filename".zip

## Usage

``` r
reporting_save_archive(x, filepath = tempdir(), saveRDS = FALSE)
```

## Arguments

- x:

  A `list` like the one created by function
  `reporting_produce_data_object_v2.0`

- filepath:

  A `character` file path. Defaults to temporary directory

- saveRDS:

  A `logical`. Save also object in RDS format. Defaults to FALSE

## Value

named A `list` containing paths to saved files filepaths. Slots are
named "zip" and possibly "RDS". A path to the zip file, or rds file, can
be defined by the parameter `filepath`.

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

archive <- reporting_save_archive(
  x = research_object,
  # obtained from the function `reporting_produce_data_object_v2.0()`
  filepath = ".",
  saveRDS = TRUE
)

## End (Not run)
} # }
```
