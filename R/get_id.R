#' @title eLTER get_id function
#' @description Internal function to retrieve json content from ID
#' @param deimsid A `character`. It is the DEIMS ID of the site, activity or
#' dataset from DEIMS-SDR website. More information about DEIMS ID from:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @param resource Character: one among `"sites"` (default), `"activities"` or
#'  `"datasets"` (`"networks"` currently not tested).
#' @param test Logical: if TRUE, content is not retrieved from
#'  \url{https://deims.org} but internally (only for some defined IDs).
#'  Used for testing purposes to avoid errors in case or HTTP error 500.
#'  If missing, the content of the internal variable
#' @param ... Arguments to be passed to `httr::RETRY()`.
#' @param jj a character
#' @return A character containing the json content, and an attribute `status`.
#' @importFrom dtplyr lazy_dt
#' @importFrom dplyr as_tibble
#' @importFrom httr content RETRY
#' @importFrom jqr jq
#' @importFrom jsonlite stream_in
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2021) \email{luigi@@ranghetti.info}
#' @keywords internal
#'
get_id <- function(deimsid, resource = "sites", test, ...) {

  # Check test mode
  if (missing(test)) {
    test <- Sys.getenv("LOCAL_DEIMS") == TRUE
  }

  deimsid <- sub("^.+/", "", deimsid)

  # Test mode
  if (test == TRUE) {

    # Define IDs which can be used internally
    valid_ids <- gsub("\\.rds$", "", list.files(
      system.file(file.path("deimsid", resource), package = "ReLTER"),
      "\\.rds$"
    ))
    if (!deimsid %in% valid_ids) {
      stop(paste0(
        "deimsid '", deimsid,
        "' cannot be used in test mode with resource = '", resource, "'."
      ))
      # # code to store locally
      # if (resource == "networks") {
      #   # for Network
      #   url <- paste0("https://deims.org/api/sites?network=", deimsid)
      # } else {
      #   # for other entities
      #   url <- file.path("https://deims.org/api", resource, deimsid)
      # }
      # export <- httr::RETRY("GET", url = url, ...)
      # jj <- suppressMessages(httr::content(
      #         export, "text", encoding = "UTF-8")
      #       )
      # saveRDS(jj, file.path(
      #   system.file(file.path("deimsid", resource), package = "ReLTER"),
      #   paste0(deimsid, ".rds")
      # ))
    }

    # Retrieve the content locally
    jj <- readRDS(file.path(
      system.file(file.path("deimsid", resource), package = "ReLTER"),
      paste0(deimsid, ".rds")
    ))

  } else {

    if (resource == "networks") {
      url <- paste0("https://deims.org/api/sites?network=", deimsid)
    } else {
      url <- file.path("https://deims.org/api", resource, deimsid)
    }

    # Normal mode
    export <- httr::RETRY("GET", url = url, ...)
    jj <- suppressMessages(httr::content(export, "text", encoding = "UTF-8"))

  }

  status <- jj %>%
    jqr::jq(as.character("{status: .errors.status}")) %>%
    textConnection() %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE, verbose = FALSE) %>%
    dtplyr::lazy_dt() %>%
    dplyr::as_tibble()

  attr(jj, "status") <- status
  jj

}
