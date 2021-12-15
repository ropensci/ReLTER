#' @title eLTER get_site_affiliations function
#' @description This function obtains details about an eLTER site
#' through the DEIMS-SDR sites API.
#' @param deimsid A character. The DEIMS ID of the site from
#' DEIMS-SDR website. More information about DEIMS ID in this pages:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the affiliations information, such as: networks and projects in
#' which the site is involved.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr RETRY content
#' @importFrom jqr jq
#' @importFrom jsonlite stream_in
#' @importFrom dtplyr lazy_dt
#' @importFrom dplyr as_tibble
#' @importFrom utils capture.output
#' @export
#' @keywords internal
#' @examples
#' tSiteAffiliation <- get_site_affiliations(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' )
#' tSiteAffiliation
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
  jj <- ReLTER:::get_id(deimsid, "sites")
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
