#' @title eLTER do_Q function
#' @description This function parses json content returned from an HTTP request
#' @param q a character
#' @param jj a character
#' @return The output of the function is ...
#' @author Paolo Tagliolato, PhD (2021) \email{tagliolato.p@@irea.cnr.it}
#' @importFrom jsonlite stream_in
#' @importFrom jqr jq
#' @importFrom dtplyr lazy_dt
#' @importFrom magrittr %>%
#' @keywords internal
#' @examples
#' deimsid <- "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' q <- '{title: .title,
#'   uri: "\\(.id.prefix)\\(.id.suffix)",
#'   geoCoord: .attributes.geographic.coordinates,
#'   country: .attributes.geographic.country,
#'   geoElev: .attributes.geographic.elevation,
#'   affiliation: .attributes.affiliation
#' }'
#' jj <- ReLTER:::get_id(deimsid, "sites")
#' ReLTER:::do_Q(q, jj)
#'
### function do_q
do_Q <- function(q, jj) {
  jj %>%
    jqr::jq(as.character(q)) %>%
    textConnection(encoding = "UTF-8") %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE, verbose = FALSE) %>%
    dtplyr::lazy_dt()
}
