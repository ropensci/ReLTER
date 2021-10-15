#' @title eLTER getSiteAffiliations function
#' @description This function allows to obtain the information about
#' affiliations of the eLTER site througth the DEIMS-SDR sites API.
#' @param deimsid A character. It is the DEIMS iD of network make from
#' DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the affiliations information, such as: networks and projects in
#' which the site is involved.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import
#' @importFrom httr GET content
#' @importFrom jqr jq
#' @importFrom jsonlite stream_in
#' @importFrom dtplyr lazy_dt
#' @importFrom dplyr as_tibble
#' @importFrom utils capture.output
#' @export
#' @examples
#' tSiteAffiliation <- getSiteAffiliations(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' )
#' tSiteAffiliation
#'
### function getSiteAffiliations
getSiteAffiliations <- function(deimsid) {
  q <- '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       affiliation: .attributes.affiliation
      }'
  url <- paste0(
    "https://deims.org/",
    "api/sites/",
    sub("^.+/", "", deimsid)
  )
  export <- httr::GET(url = url)
  jj <- suppressMessages(httr::content(export, "text"))
  status <- jj %>% 
    jqr::jq(as.character('{status: .errors.status}')) %>% 
    textConnection() %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE) %>%
    dtplyr::lazy_dt() %>% 
    dplyr::as_tibble()
  if (is.na(status)) {
    invisible(
      utils::capture.output(
        affiliations <- dplyr::as_tibble(
          ReLTER::do_Q(q, jj)
        )
      )
    )
  } else {
    message("\n---- The requested page could not be found. Please check again the DEIMS.iD ----\n")
    affiliations <- NULL
  }
  affiliations
}
