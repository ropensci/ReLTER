#' Get version of DEIMS-SDR API
#' @description `r lifecycle::badge("stable")`
#' This function obtains the version of the DEIMS-SDR API.
#' @param deims_url A `character`. DEIMS-SDR base URL. Defaults to package
#' settings.
#' @return version number.
#' @importFrom httr2 request req_headers req_retry
#' @importFrom httr2 req_perform resp_check_status resp_body_string
#' @importFrom dplyr as_tibble pull
#' @export
get_deims_API_version <- function(deims_url = get_deims_base_url()) {
  if (!endsWith(deims_url, "/")) {
    deims_url <- paste0(deims_url, "/")
  }
  api_url <- paste0(deims_url, "api")
  q <- "{version: .info.version}"
  export <- httr2::request(api_url) %>%
    httr2::req_method("GET") %>%
    httr2::req_headers(Accept = "application/json") %>%
    httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
    httr2::req_perform()
  httr2::resp_check_status(export)
  jj <- httr2::resp_body_string(export) # already UTF-8 encoded
  apiversion <- do_Q(q, jj) %>% dplyr::as_tibble() %>% dplyr::pull(version)
  return(apiversion)
}


