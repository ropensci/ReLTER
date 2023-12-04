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
    "perc",
    "prod_opt_list"
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
#' @importFrom httr2 request req_method req_headers
#' @importFrom httr2 req_retry req_perform resp_check_status
set_deims_base_url <- function(url = "https://deims.org/", force = FALSE) {
  if (!endsWith(url, "/")) {
    url <- paste0(url, "/")
  }
  export <- httr2::request(url) %>%
    httr2::req_method("GET") %>%
    httr2::req_headers(Accept = "application/html") %>%
    httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
    httr2::req_perform()
  httr2::resp_check_status(export)

  if (!export$status_code == 200) {
    if (force) {
      warning("The URL ", url, " is not reachable, I set it because
              force TRUE is specified")
    } else  {
      stop("The URL ", url, " is not reachable")
    }
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
