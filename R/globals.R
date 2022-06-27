# removes false positives from the check
utils::globalVariables(
  c(
    "freq",
    "hjust",
    "label",
    "middle",
    "n",
    "parameterGroups",
    "vjust",
    "end",
    "start"
  )
)

#' Package settings that can be changed by the user
#' @export
#' @family package_customizable_settings
#' @export
package_settings <- (function() {
  pe <- new.env()
  deimsBaseUrl <- "https://deims.org/"
  assign("deimsBaseUrl", deimsBaseUrl, envir = pe)
  pe
})()

#' set deims api base url
#' @param url A `character`. Set the base url to deims.
#' @export
#' @importFrom RCurl url.exists
setDeimsBaseUrl <- function(url = "https://deims.org/", force = FALSE) {
  if (!endsWith(url, "/")) {
    url <- paste0(url, "/")
  }
  if (!RCurl::url.exists(url)) {
    if (force) {
      warning("The url ", url, " is not reachable, I set it because 
              force TRUE is specified")
    }
    stop("The url ", url, " is not reachable")
  }
  message("Changing deims base url to: ", url)
  assign("deimsBaseUrl", url, envir = package_settings)
}

#' get deims base url
#' @return deims base url
#' @family package_customizable_settings
#' @export
getDeimsBaseUrl <- function() {
  get("deimsBaseUrl", envir = package_settings)
}
