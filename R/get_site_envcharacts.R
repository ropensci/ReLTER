#' @title eLTER get_site_envcharacts function
#' @description This function obtains Environmental Characteristics
#' of an eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. The DEIMS ID of a site from
#' DEIMS-SDR website. More information about DEIMS I from:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the environmental characteristics where available,
#' such as: air temperature, precipitation, biogeographical region, biome,
#' ecosystem land use, EUNIS habitat, geoBon biome, geology, hydrology, soils
#' and vegetation.
#' @author Alessandro Oggioni, phD (2021) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr RETRY content
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @export
#' @keywords internal
#' @examples
#' tSiteEnvCharacts <- get_site_envcharacts(
#'   deimsid = "https://deims.org/17210eba-d832-4759-89fa-9ff127cbdf6e"
#' )
#' tSiteEnvCharacts
#'
### function get_site_envcharacts
get_site_envcharacts <- function(deimsid) {
  q <- '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       envCharacteristics: .attributes.environmentalCharacteristics
      }'
  jj <- get_id(deimsid, "sites")
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        envCharacteristics <- dplyr::as_tibble(
          do_Q(q, jj)
        )
      )
    )
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS.iD\n----\n")
    envCharacteristics <- NULL
  }
  envCharacteristics
}
