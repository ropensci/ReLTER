#' get version of DEIMS api
#' @param deims_url A `character`. DEIMS base url. Defaults to package 
#' settings.
#' @return version number.
#' @importFrom httr RETRY content
#' @importFrom dplyr as_tibble pull
#' @export
get_deims_API_version <- function(deims_url = getDeimsBaseUrl()) {
  api_url <- paste0(deims_url, "api")
  q <- "{version: .info.version}"
  export <- httr::RETRY("GET", url = api_url)
  jj <- suppressMessages(httr::content(export, "text", encoding = "UTF-8"))
  
  apiversion <- do_Q(q, jj) %>% dplyr::as_tibble() %>% dplyr::pull(version)
  
  return(apiversion)
}
