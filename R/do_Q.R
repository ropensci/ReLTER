#' @title eLTER do_Q function
#' @description This function parses json content returned from an HTTP request
#' @param q a character
#' @param jj a character
#' @return The output of the function is ...
#' @author Paolo Tagliolato, phD (2021) \email{tagliolato.p@@irea.cnr.it}
#' @importFrom jsonlite stream_in
#' @importFrom jqr jq
#' @importFrom dtplyr lazy_dt
#' @keywords internal
#' @examples
#' deimsid <- "https://deims.org/17210eba-d832-4759-89fa-9ff127cbdf6e"
#' url <- paste0("https://deims.org/", "api/sites/", substring(deimsid, 19))
#' export <- httr::GET(url = url)
#' jj <- httr::content(export, "text")
#'
#' q <- '{title: .title,
#'     uri: "\\(.id.prefix)\\(.id.suffix)",
#'     geoBoundaries: .attributes.geographic.boundaries,
#'     geoCoord: .attributes.geographic.coordinates,
#'     country: .attributes.geographic.country,
#'     geoElev: .attributes.geographic.elevation,
#'     envCharacteristics: .attributes.environmentalCharacteristics
#'   }'
#'
#' do_Q(q, jj)
#'
### function do_q
do_Q <- function(q, jj) {
  require("dplyr")
  jj %>%
    jqr::jq(as.character(q)) %>%
    textConnection() %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE) %>%
    dtplyr::lazy_dt()
}
