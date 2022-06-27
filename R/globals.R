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
#' @keywords internal
package_settings <- (function() {
  pe<-new.env()
  deimsBaseUrl <- "https://deims.org/"
  assign("deimsBaseUrl", deimsBaseUrl, envir = pe)
})()

#' set deims api base url
#' @param url A `character`. Set the base url to deims.
#' @export
#' @importFrom RCurl url.exists
#' @keywords internal
setDeimsBaseUrl <- function(url = "https://deims.org/", force = FALSE) {
  if(!endsWith(url,"/")) {
    url <- paste0(url,"/")
  }
  if (!RCurl::url.exists(url)) {
    if (force) {
      warning("The url is not reachable, I set it because 
              force TRUE is specified")
    }
    error("The url is not reachable")
  }
  assign("deimsBaseUrl", url, envir = package_settings)
}

#' get deims base url
#' @return deims base url
#' @family package_customizable_settings
#' @keywords internal
getDeimsBaseUrl <- function() {
  get("deimsBaseUrl", envir = package_settings)
}
