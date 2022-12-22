#' Obtain the data from a dataset deposited in Zenodo record.
#' @description `r lifecycle::badge("experimental")`
#' The function returns the dataset, or file(s), deposited in Zenodo record.
#' @param doi A `character`. It is the DOI of the Zenodo record.
#' @param rdata_exist A `logical`. Is the .RData or .rds file in the record
#' we are questioning? Default TRUE.
#' @return a file(s) containing in the Zenodo record.
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @importFrom zen4R ZenodoManager
#' @importFrom stringr str_replace_all
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#' 
#' record <- get_zenodo_data(
#'   doi = "10.5281/zenodo.7041152", # test dataset
#'   rdata_exist = TRUE
#' )
#' 
#' ## End (Not run)
#' }
#'
### function get_zenodo_data
get_zenodo_data <- function(doi, rdata_exist = TRUE) {
  # open instance to Zenodo repository ----
  zenodo <- zen4R::ZenodoManager$new(logger = "INFO")
  # read record by DOI ----
  if (grepl("zenodo" , doi)) {
    rec <- zenodo$getRecordByDOI(doi)
    record_type <- rec$metadata$upload_type
    if (record_type != "dataset") {
      results <- NULL
      message("\n----\nThe record refered to the DOI is not a dataset.
              Please provide a DOI of Zenodo dataset.\n----\n")
    } else {
      record_id <- stringr::str_replace_all(rec$record_id, "[./]", "_")
      files <- rec$listFiles(pretty = TRUE)
      # create a folder where to download the files ----
      dir_name <- paste0("download_files_of_", record_id)
      if (!dir.exists(dir_name)) {
        dir.create(dir_name)
      }
      rec$downloadFiles(path = dir_name)
      # download bib reference of record ----
      bib_name <- paste0(dir_name, "/", record_id)
      rec$exportAsBibTeX(filename = bib_name)
      # return dataset from RData downloaded ----
      row_rdataPath <- which(grepl("RData", files$filename))
      if (isTRUE(rdata_exist) || length(row_rdataPath) > 0) {
        rdata_file <- files$filename[row_rdataPath]
        loading <- function(rdata_file)
        {
          e <- new.env()
          load(rdata_file, envir = e)
          e
        }
        
        results <- loading(paste0(dir_name, "/", rdata_file))
        message(
          paste0(
            "\n----\n",
            "The file containing data '.RData' are downloaded in '", dir_name,
            "' folder.\n",
            "The data can use in this environment thanks to the results object.\n\n",
            "Please remember to cite the downloaded dataset using the file '",
            paste0(bib_name, "_BibTeX.bib'"),
            "\n----\n"
          )
        )
        ls.str(results)
      } else {
        results <- NULL
        message(
          paste0(
            "\n----\n",
            "The Zenodo record with DOI ", doi,
            " doesn't contains '.RData' file.\n",
            "This function can produce the results only from '.RData'.\n\n",
            "Anyway the files are downloaded in ", dir_name, " folder.\n\n",
            "Please remember to cite the downloaded dataset using the file '",
            paste0(bib_name, ".bib'"),
            "\n----\n"
          )
        )
        return(results)
      }
    }
  } else {
    results <- NULL
    message("\n----\nDOI Not Found. Please
check again the Zenodo DOI.\n----\n")
  }
}
