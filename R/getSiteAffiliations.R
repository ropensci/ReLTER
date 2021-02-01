#' @title eLTER_getSiteAffiliations
#' @description This function allows to obtain the information about affiliations of the eLTER site througth the DEIMS-SDR sites API.
#' @param deimsid
#' @return The output of the function is a tibble with main features of the site and the affiliations information, such as: networks and projects in which the site is involved.
#' @author Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
#' @import tibble httr
#' @export
#' @examples
#' getSiteAffiliations(deimsid = 'https://deims.org/17210eba-d832-4759-89fa-9ff127cbdf6e')
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
  jj <- httr::content(export, "text")
  affiliations <- tibble::as_tibble(do_Q(q, jj))
  ## TODO verify why affiliation.projects is empty
  affiliations
}
