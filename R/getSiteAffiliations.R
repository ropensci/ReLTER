#' @title eLTER_getSiteAffiliations
#' @description This function allows to obtain the information about affiliations of the eLTER site througth the DEIMS-SDR sites API.
#' @param deimsid is a DEIMS iD of network make from DEIMS-SDR website. More information about DEIMS iD in this page https://deims.org/docs/deimsid.html.
#' @return The output of the function is a tibble with main features of the site and the affiliations information, such as: networks and projects in which the site is involved.
#' @author Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
#' @import tibble httr
#' @export
#' @examples
#' getSiteAffiliations(deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe')
#'
### function getSiteAffiliations
getSiteAffiliations <- function(deimsid) {
  q = '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       affiliation: .attributes.affiliation
      }'
  url <- paste0("https://deims.org/", "api/sites/", substring(deimsid, 19))
  export <- httr::GET(url = url)
  jj <- suppressMessages(httr::content(export, "text"))
  invisible(capture.output(affiliations <- tibble::as_tibble(ReLTER::do_Q(q, jj))))
  ## TODO verify why affiliation.projects is empty
  affiliations
}
