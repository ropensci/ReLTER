#' Obtain the data from a dataset deposited in Zenodo record.
#' @description `r lifecycle::badge("experimental")`
#' The function download the file(s) deposited in Zenodo record and returns
#' a tibble with metadata.
#' @param doi A `character`. It is the DOI of the Zenodo record.
#' @param rdata_exist A `logical`. Is the .RData or .rds file in the record
#' we are questioning? Default TRUE.
#' @return a file(s) containing in the Zenodo record and tibble contains some
#' metadata of Zenodo record.
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @importFrom tibble tibble enframe
#' @importFrom stringr str_replace_all
#' @importFrom lubridate as_date
#' @importFrom dplyr mutate
#' @importFrom units set_units
#' @seealso [zen4R::ZenodoManager()]
#' @seealso [tidyr::unnest()]
#' @seealso [tidyr::nest()]
#' @references
#'   \insertRef{tibbleR}{ReLTER}
#'   
#'   \insertRef{stringrR}{ReLTER}
#'   
#'   \insertRef{lubridateR}{ReLTER}
#'   
#'   \insertRef{dplyrR}{ReLTER}
#'   
#'   \insertRef{unitsR}{ReLTER}
#'   
#'   \insertRef{zen4R}{ReLTER}
#'   
#'   \insertRef{tidyr}{ReLTER}
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#' 
#' record <- get_zenodo_data(
#'   doi = "10.5281/zenodo.7041152", # test dataset
#'   rdata_exist = TRUE
#' )
#' record
#' 
#' ## End (Not run)
#' }
#'
### function get_zenodo_data
get_zenodo_data <- function(doi, rdata_exist = TRUE) {
  # Check if required packages are installed
  if (!requireNamespace("zen4R", quietly = TRUE)) {
    stop(
      "\n----\nThe function 'get_zenodo_data()' requires the optional package 'zen4R'.\n",
      "Please install it with: install.packages(\"zen4R\")\n----\n"
    )
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop(
      "\n----\nThe function 'get_zenodo_data()' requires the optional package 'tidyr'.\n",
      "Please install it with: install.packages(\"tidyr\")\n----\n"
    )
  }
  # open instance to Zenodo repository ----
  ZenodoManager_fx <- getNamespace("zen4R")$ZenodoManager$new
  zenodo <- ZenodoManager_fx(logger = "INFO")
  # read record by DOI ----
  if (grepl("zenodo" , doi)) {
    rec <- zenodo$getRecordByDOI(doi)
    record_type <- rec$metadata$resource_type$type
    if (record_type != "dataset") {
      dataset_info <- NULL
      message("\n----\nThe record refered to the DOI is not a dataset.
              Please provide a DOI of Zenodo dataset.\n----\n")
    } else {
      record_id <- stringr::str_replace_all(rec$id, "[./]", "_")
      unest_fx <- getNamespace("tidyr", "unnest")
      nest_fx <- getNamespace("tidyr", "nest")
      results <- tibble::tibble(
        id = record_id,
        title = rec$metadata$title,
        created = lubridate::as_date(rec$metadata$publication_date),
        updated = NA,
        doi = rec$metadata$doi,
        community = tibble::tibble(
          id = sapply(rec$metadata$communities, `[[`, "id")
        ) %>%
          unest_fx(cols = c(id)) %>%
          nest_fx(),
        creators = tibble::tibble(
          name = sapply(rec$metadata$creators, `[[`, "name"),
          affiliation = sapply(rec$metadata$creators, `[[`, "affiliation"),
          orcid = sapply(rec$metadata$creators, `[[`, "orcid")
        ) %>%
          unest_fx(cols = c(name, affiliation, orcid)) %>%
          nest_fx(),
        contact_email = NA,
        description = rec$metadata$description,
        ePIC_PID = NA,
        keywords = tibble::enframe(paste(rec$metadata$keywords)) %>%
          nest_fx(),
        open_access = rec$metadata$access_right,
        license = rec$metadata$license$id,
        owners = NA,
        publication_date = lubridate::as_date(rec$metadata$publication_date),
        publication_state = NA,
        type = record_type,
        files = rec$listFiles(pretty = TRUE) %>%
          nest_fx()#,
        # metadata_URL = NA
      )
      results$files$data[[1]] <- results$files$data[[1]] %>%
        dplyr::mutate(
          filesize = units::set_units(
            as.numeric(filesize),
            "bytes"
          )
        )
      # create a folder where to download the files ----
      dir_name <- paste0("Zenodo_files_of_", record_id)
      if (!dir.exists(dir_name)) {
        dir.create(dir_name)
      }
      rec$downloadFiles(path = dir_name)
      # download bib reference of record ----
      bib_name <- paste0(dir_name, "/", record_id)
      rec$exportAs("BibTeX", filename = bib_name)
      # return dataset from RData downloaded ----
      row_rdataPath <- which(grepl("RData", results$files$data[[1]]$filename))
      if (isTRUE(rdata_exist) || length(row_rdataPath) > 0) {
        rdata_file <- results$files$data[[1]]$filename[row_rdataPath]
        loading <- function(rdata_file)
        {
          e <- new.env()
          load(rdata_file, envir = e)
          e
        }
        dataset_info <- loading(paste0(dir_name, "/", rdata_file))
        message(
          paste0(
            "\n----\n",
            "The file containing data '.RData' are downloaded in '", dir_name,
            "' folder.\n",
            "The data can use in this environment thanks to the results object.",
            # "\n\nPlease remember to cite the downloaded dataset using the file '",
            # paste0(bib_name, "_BibTeX.bib'"),
            "\n----\n"
          )
        )
        ls.str(dataset_info)
        results
      } else {
        dataset_info <- NULL
        message(
          paste0(
            "\n----\n",
            "The Zenodo record with DOI ", doi,
            " doesn't contains '.RData' file.\n",
            "This function can produce the results only from '.RData'.\n\n",
            "Anyway the files are downloaded in ", dir_name, " folder.",
            # "\n\nPlease remember to cite the downloaded dataset using the file '",
            # paste0(bib_name, ".bib'"),
            "\n----\n"
          )
        )
        ls.str(dataset_info)
        results
      }
    }
  } else {
    dataset_info <- NULL
    results <- NULL
    message("\n----\nDOI Not Found. Please
check again the Zenodo DOI.\n----\n")
  }
}
