#' eLTER write Rdata data
#' @description `r lifecycle::badge("experimental")`
#' This function write a Rdata file from csv, tsv, txt, xls or xlsx
#' dataset
#' @param myfiles A `character`. The list of the files to deposit in Zenodo.
#' Please provide all files only with 'csv' extension.
#' @param delim A `character`. Provide the character used to separate fields
#' within a record. Only if the extension of the file(s) are 'csv', 'tsv', or
#' 'txt'.
#' @return This function returns a rds files.
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @importFrom readr read_delim
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#' 
#' elter_write_rdata(
#'   myfiles = c(
#'    "miscellaneus/file_show/data_mapping.csv",
#'    "miscellaneus/file_show/reference_TAXA.csv",
#'    "miscellaneus/file_show/reference_VARIABLES.csv"
#'   ),
#'   delim = ";"
#' )
#' }
#' 
#' ## End (Not run)
#'
### function elter_write_rdata
elter_write_rdata <- function(myfiles, delim) {
  exts <- strsplit(basename(myfiles), split = "\\.")
  num_files <- length(myfiles)
  if (!all(grepl("\\.csv$", myfiles))) {
    message(
      paste0(
        "\n----\n",
        "Please provide all files with 'csv' extension.",
        "\n----\n"
      )
    )
  } else {
    files_name <- as.character()
    for (i in seq_len(num_files)) {
      name <- paste0(exts[[i]][1])
      assign(
        name,
        readr::read_delim(
          myfiles[i],
          delim = delim,
          show_col_types = FALSE
        )
      )
      files_name <- append(files_name, name)
    }
    save(
      list = files_name,
      file = "data.RData"
    )
  }
}
