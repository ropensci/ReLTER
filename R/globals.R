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
    "start",
    "perc"
  )
)

#' Package settings that can be changed by the user
#' @family package_customizable_settings
#' @export
package_settings <- (function() {
  pe <- new.env()
  deimsBaseUrl <- "https://deims.org/"
  assign("deimsBaseUrl", deimsBaseUrl, envir = pe)
  pe
})()

#' Set DEIMS-SDR API base URL
#' @param url A `character`. Set the base URL to DEIMS-SDR.
#' @param force A `boolean`. Default FALSE.
#' @export
#' @importFrom RCurl url.exists
set_deims_base_url <- function(url = "https://deims.org/", force = FALSE) {
  if (!endsWith(url, "/")) {
    url <- paste0(url, "/")
  }
  if (!RCurl::url.exists(url)) {
    if (force) {
      warning("The URL ", url, " is not reachable, I set it because
              force TRUE is specified")
    }
    stop("The URL ", url, " is not reachable")
  }
  message("Changing DEIMS-SDR base URL to: ", url)
  assign("deimsBaseUrl", url, envir = package_settings)
}

#' Get DEIMS-SDR base URL
#' @return DEIMS-SDR base URL
#' @family package_customizable_settings
#' @export
get_deims_base_url <- function() {
  get("deimsBaseUrl", envir = package_settings)
}
