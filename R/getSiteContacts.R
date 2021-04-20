#' @title eLTER getSiteContact function
#' @description This function allows to obtain the contact information of the eLTER site througth the DEIMS-SDR sites API.
#' @param deimsid A `character`. It is the DEIMS iD of network make from DEIMS-SDR website. More information about DEIMS iD in this \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features of the site and the contact information, such as: site manager, operation organaization, metadata provider, founding agency and site url.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import tibble httr
#' @export
#' @examples
#' tSiteContact <- getSiteContact(deimsid = 'https://deims.org/17210eba-d832-4759-89fa-9ff127cbdf6e')
#' tSiteContact
#'
### function getSiteContact
getSiteContact <- function(deimsid) {
  q = '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       generalInfo: .attributes.contact
      }'
  url <- paste0("https://deims.org/", "api/sites/", substring(deimsid, 19))
  export <- httr::GET(url = url)
  jj <- suppressMessages(httr::content(export, "text"))
  invisible(capture.output(contact <- tibble::as_tibble(ReLTER::do_Q(q, jj))))
  contact
}
