#' @title eLTER get_network_sites function
#' @description This function retrieves a list of sites in the provided network
#' including title, date late updated, URI, and coordinates
#' @param networkDEIMSID A `character`. The DEIMS ID of the network from
#' DEIMS-SDR website. More information about DEIMS network ID from these pages:
#' \href{https://deims.org/docs/deimsid.html}{page}, and
#' \href{https://deims.org/search?f[0]=result_type:network}{page} the
#' complete list of ILTER networks.
#'
#' @return The output of the function is a point vector of `sf` class
#' (package sf) of the network's sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_as_sf st_is_valid
#' @importFrom dplyr select as_tibble
#' @importFrom leaflet leaflet addTiles addMarkers
#' @importFrom httr RETRY content
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
  url <- paste0("https://deims.org/",
                "api/sites?network=",
                sub("^.+/", "", networkDEIMSID))
  export <- httr::RETRY("GET", url = url, times = 5)
  lterNetworkSitesCoords <- jsonlite::fromJSON(
    httr::content(export, as = "text", encoding = "UTF-8"))

  lterNetworkSitesCoords <- dplyr::as_tibble(lterNetworkSitesCoords)
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
    lSNPD_valid <- sf::st_is_valid(
      lterSitesNetworkPointDEIMS
    )
    if (any(lSNPD_valid)) {
      map <- leaflet::leaflet(lterSitesNetworkPointDEIMS) %>%
        leaflet::addTiles() %>%
        leaflet::addMarkers()
      print(map)
      return(lterSitesNetworkPointDEIMS)
    } else {
      message("\n----\nThe maps cannot be created because the coordinates,
provided in DEIMS-SDR, have invalid geometry.
Please check the content (returned by this function) and refer this error
to DEIMS-SDR contact person of the network, citing the Network ID.\n----\n")
      return(lterSitesNetworkPointDEIMS)
    }
  } else {
    message("\n----\nThe requested page could not be found.
Please check the Network ID\n----\n")
    return(NULL)
  }
}
