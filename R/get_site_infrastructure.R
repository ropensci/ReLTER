#' @title eLTER get_site_infrastructure function
#' @description This function obtains infrastructure information
#' of an eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. More information about DEIMS ID from:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features of the
#' site and infrastructure information where available, such as:
#' power supply, accessibility, maintenaince interval, etc.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr RETRY content
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @keywords internal
#' @examples
#' tSiteInfrastructure <- get_site_infrastructure(
#'   deimsid = "https://deims.org/17210eba-d832-4759-89fa-9ff127cbdf6e"
#' )
#' tSiteInfrastructure
#'
### function get_site_infrastructure
get_site_infrastructure <- function(deimsid) {
  q <- '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       generalInfo: .attributes.infrastructure
      }'
  jj <- ReLTER:::get_id(deimsid, "sites")
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        infrastructure <- dplyr::as_tibble(
          do_Q(q, jj)
        )
      )
    )
    colnames(infrastructure$generalInfo.collection[[1]]) <- c(
      "collectionLabel",
      "collectionURI"
    )
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS.iD\n----\n")
    infrastructure <- NULL
  }
  infrastructure
}
