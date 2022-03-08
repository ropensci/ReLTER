#' @title eLTER get_site_affiliations function
#' @description This function obtains details about an eLTER site
#' through the DEIMS-SDR sites API.
#' @param deimsid A character. The DEIMS.iD of the site from
#' DEIMS-SDR website. DEIMS.iD information 
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the affiliations information, such as: networks and projects in
#' which the site is involved.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr RETRY content
#' @importFrom jqr jq
#' @importFrom jsonlite stream_in
#' @importFrom dplyr as_tibble
#' @importFrom utils capture.output
#' @keywords internal
#'
### function get_site_affiliations
get_site_affiliations <- function(deimsid) {
  q <- '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       affiliation: .attributes.affiliation
      }'
  jj <- get_id(deimsid, "sites")
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        affiliations <- dplyr::as_tibble(
          do_Q(q, jj)
        )
      )
    )
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS.iD\n----\n")
    affiliations <- NULL
  }
  affiliations
}
