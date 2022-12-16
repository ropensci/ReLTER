#' eLTER get_id function
#' @description Internal function to retrieve json content from ID
#' @param deimsid A `character`. It is the DEIMS ID of the site, activity or
#' dataset from DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
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
#' @author Paolo Tagliolato, PhD (2021) \email{tagliolato.p@@irea.cnr.it}
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
      if(Sys.getenv("DEV_MODE")!="" && Sys.getenv("DEV_MODE")=="TRUE"){
        .save_id(resource, deimsid, development = TRUE)
      }
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
    deimsbaseurl <- get_deims_base_url()
    if (resource == "networks") {
      url <- paste0(deimsbaseurl, "api/sites?network=", deimsid)
    } else {
      url <- file.path(deimsbaseurl, "api", resource, deimsid)
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

#' INTERNAL FUNCTION for package development only. Add new internal
#' json for local tests.
#' @param deimsid A `character`. It is the DEIMS ID (without url prefix)
#' of the site, activity or dataset from DEIMS-SDR website. DEIMS ID
#' information \href{https://deims.org/docs/deimsid.html}{here}.
#' @param resource Character: one among `"sites"`, `"activities"` or
#'  `"datasets"` (`"networks"` currently not tested).
#' @note for development, in order to add new deims resources, set
#' the environment variables "DEV_MODE" and "RELTER_DEV_INST_PATH"
#' e.g.
#' Sys.setenv("RELTER_DEV_INST_PATH",file.path(getwd(), "inst"))
#' in order to silently enable cache creation of missing resources while
#' testing the package during development
#' @noRd
.save_id <- function(resource, deimsid, development=FALSE, ...) {
  message("This function is intended for development purposes only.")
  if(!development) warning("Caching a deims entity in the package internal folder")
  
  # code to store locally
  if (resource == "networks") {
    # for Network
    url <- paste0("https://deims.org/api/sites?network=", deimsid)
  } else {
    # for other entities
    url <- file.path("https://deims.org/api", resource, deimsid)
  }
  export <- httr::RETRY("GET", url = url, ...)
  jj <- suppressMessages(httr::content(
          export, "text", encoding = "UTF-8")
        )
  
  path <- 
    if(development) file.path("inst","deimsid", resource) else file.path(
    system.file(file.path("deimsid", resource), package = "ReLTER"))
  
  if(Sys.getenv("DEV_MODE")!="" && Sys.getenv("DEV_MODE")=="TRUE" && dir.exists(Sys.getenv("RELTER_DEV_INST_PATH"))){
    path <- file.path(Sys.getenv("RELTER_DEV_INST_PATH"), "deimsid", resource)
    if(!dir.exists(path)) dir.create(path)
  }
  
  if(!dir.exists(path)) 
      stop("you are trying to save in a folder that does not exist on your computer: ", path)
  
  saveRDS(jj, file.path(path, paste0(deimsid, ".rds")))
}

.recreate_deims_cache<-function(development=TRUE){
  message("This function is intended for development purposes only.")
  if(!development) warning("Recreating all the DEIMS object cache in the package internal folder.")
  
  path <- 
    if(development) file.path("inst","deimsid") else file.path(
      system.file(file.path("deimsid"), package = "ReLTER"))
  
  if(!dir.exists(path)) 
    stop("you are trying to save in a folder that does not exist on your computer: ", path)
  
  allfilenames <- dir(path = path, recursive = T)
  myfun<-function(resource, deimsid, ...){
    message("executing .save_id(", resource,",", deimsid,")")
    .save_id(resource, deimsid, development, ...)
  }
  objs<-strcapture("(?<resource>.*)/(?<deimsid>.*)\\.rds", allfilenames, proto = data.frame(resource=character(), deimsid=character()), perl=T) %>% 
    dplyr::as_tibble()
  objs %>% purrr::pmap(myfun)
  
}
