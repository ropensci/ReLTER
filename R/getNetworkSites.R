#' @title eLTER_spatial_points_of_sites
#' @description This function provide a SpatialPointsDataFrame (sp) of sites in a network provided by DEIMS-SDR.
#' @param networkDEIMSID is a DEIMS iD of network make from DEIMS-SDR website. More information about DEIMS iD in this page https://deims.org/docs/deimsid.html, and at this page https://deims.org/search?f%5B0%5D=result_type%3Anetwork the complete list of iLTER networks.
#' @return The output of the function is SpatialPointsDataFrame (package sp).
#' @author Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
#' @import jsonlite sf
#' @export
#' @examples
#' getNetworkSites(networkDEIMSID = 'https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3')
#'
### function getNetworkSites
getNetworkSites <- function(networkDEIMSID) {
  lterNetworkSitesCoords <- jsonlite::fromJSON(paste0("https://deims.org/", "api/sites?network=", substring(networkDEIMSID, 27)))
  lterSitesNetworkPointDEIMS <- # sf::as_Spatial(
    sf::st_as_sf(lterNetworkSitesCoords, wkt = 'coordinates')
    # )
  lterSitesNetworkPointDEIMS
}

