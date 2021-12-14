#' @title eLTER get_id function
#' @description Internal function to retrieve json content from ID
#' @param deimsid A `character`. It is the DEIMS ID of the site from
#' DEIMS-SDR website. More information about DEIMS ID from:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @param resource Character: one among `"sites"` (default), `"activities"` or
#'  `"datasets"` (`"networks"` currently mnot tested).
#' @param test Logical: if TRUE, content is not retrieved from 
#'  \url{https://deims.org} but internally (only for some defined IDs).
#'  Used for testing purposes to avoid errors in case or HTTP error 500.
#'  If missing, the content of the internal variable
#' @param ... Arguments to be passed to `httr::RETRY()`.
#' @param jj a character
#' @return A character containing the json content, and an attribute `status`.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2021) \email{luigi@@ranghetti.info}
#' @keywords internal
#' @examples
#' deimsid <- "f30007c4-8a6e-4f11-ab87-569db54638fe"
#' jj <- ReLTER:::get_id(deimsid, "sites", times = 5)
#' 
#' # Test ode (only for defined IDs)
#' jj <- ReLTER:::get_id(deimsid, "sites", test = TRUE)
#'
get_id <- function(deimsid, resource = "sites", test, ...) {
  
  # Check test mode
  if (missing(test)) {
    test <- Sys.getenv('LOCAL_DEIMS') == TRUE
  }
  
  deimsid <- sub("^.+/", "", deimsid)
  
  # Test mode
  if (test == TRUE) {
    
    # Define IDs which can be used internally
    valid_ids <- gsub(
      "\\.json$", "",
      list.files(system.file("deimsid", package="ReLTER"), "\\.json$")
    )
    if (!deimsid %in% valid_ids) {
      stop(paste0(
        "deimsid '",deimsid,"' cannot be used in test mode."
      ))
    }
    
    # Retrieve the content locally
    jj <- readLines(file.path(
      system.file("deimsid", package="ReLTER"), 
      paste0(deimsid, ".json")
    ))
    
  } else {
    
    # Normal mode
    url <- file.path("https://deims.org/api", resource, deimsid)
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
