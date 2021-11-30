#' @title eLTER get_network_sites function
#' @description This function retrieves a list of sites in the provided network
#' including title, date late updated, URI, and coordinates
#' @param networkDEIMSID A `character`. The DEIMS ID of the network from
#' DEIMS-SDR website. More information about DEIMS network ID from these pages:
#' \href{https://deims.org/docs/deimsid.html}{page}, and
#' \href{https://deims.org/search?f[0]=result_type:network}{page} the
#' complete list of ILTER networks.
#' @return The output of the function is `SpatialPointsDataFrame` (package sp)
#' of the network's sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_as_sf
#' @importFrom dplyr select as_tibble
#' @importFrom leaflet leaflet addTiles addMarkers
#' @export
#' @examples
#' \dontrun{
#' listSites <- get_network_sites(
#'   networkDEIMSID =
#'   "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
#' )
#' listSites[1:10, ]
#' }
#'
### function get_network_sites
get_network_sites <- function(networkDEIMSID) {
  lterNetworkSitesCoords <- jsonlite::fromJSON(
    paste0(
      "https://deims.org/",
      "api/sites?network=",
      sub("^.+/", "", networkDEIMSID)
    )
  ) %>% dplyr::as_tibble()
  if (length(lterNetworkSitesCoords) != 0) {
    lterSitesNetworkPointDEIMS <- sf::st_as_sf(
      lterNetworkSitesCoords,
      wkt = "coordinates"
    )
    lterSitesNetworkPointDEIMS$uri <- paste0(
      lterSitesNetworkPointDEIMS$id$prefix,
      lterSitesNetworkPointDEIMS$id$suffix
    )
    lterSitesNetworkPointDEIMS <- lterSitesNetworkPointDEIMS %>%
      dplyr::select(
        "title", "changed", "uri", "coordinates"
      )
    lterSitesNetworkPointDEIMS_SP <- sf::as_Spatial(
      lterSitesNetworkPointDEIMS$coordinates
    )
    lSNPD_valid <- rgeos::gIsValid(
      lterSitesNetworkPointDEIMS_SP,
      byid = FALSE,
      reason = TRUE
    )
    if (lSNPD_valid == "Valid Geometry") {
      map <- leaflet::leaflet(lterSitesNetworkPointDEIMS) %>%
        leaflet::addTiles() %>%
        leaflet::addMarkers()
      print(map)
      lterSitesNetworkPointDEIMS
    } else {
      map <- leaflet::leaflet() %>%
        leaflet::addTiles()
      message("\n----\nThe maps cannot be created because the coordinates,
provided in DEIMS-SDR, has an invalid geometry.
Please check the content and refers this error to DEIMS-SDR contact person of
the network, citing the Network.iD.\n----\n")
      lterSitesNetworkPointDEIMS
      print(map)
    }
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the Network.iD\n----\n")
    lterSitesNetworkPointDEIMS <- NULL
    map <- NULL
  }
}
