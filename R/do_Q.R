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
#'
### function do_q
do_Q <- function(q, jj) {
  jj %>%
    jqr::jq(as.character(q)) %>%
    textConnection(encoding = "UTF-8") %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE, verbose = FALSE) %>%
    dtplyr::lazy_dt()
}
