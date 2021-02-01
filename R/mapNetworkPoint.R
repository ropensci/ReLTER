#' @title eLTER_map_of_network_points
#' @description This function provide a map (image) of site LTER network started from DEIMS iD of network.
#' @param networkDEIMSID is a DEIMS iD of network make from DEIMS-SDR website. More information about DEIMS iD in this page https://deims.org/docs/deimsid.html, and at this page https://deims.org/search?f%5B0%5D=result_type%3Anetwork the complete list of iLTER networks.
#' @param countryCode follow the SO 3166-1 alpha-3 codes. This ISO convention are three-letter country codes defined in ISO 3166-1, part of the ISO 3166 standard published by the International Organization for Standardization (ISO), to represent countries, dependent territories, and special areas of geographical interest. The map produced by this function will be limited only to the contry indicated in this parameter, if the network has a extraterritorial sites those will not represented.
#' @return The output of the function is a tmap plot containing an image of geographic distribution of the network sites present in the chosen country.
#' @author Alessandro Oggioni, phD (2020) <oggioniale@gmail.com>
#' @import jsonlite sf raster rgeos tmap
#' @export
#' @examples
#' mapNetworkPoint(networkDEIMSID = 'https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3',
#' countryCode = 'ITA')
#'
### function mapNetworkPoint
mapNetworkPoint <- function(networkDEIMSID, countryCode) {
  lterNetworkSitesCoords <- jsonlite::fromJSON(paste0("https://deims.org/", "api/sites?network=", substring(networkDEIMSID, 27)))
  lterSitesNetworkPointDEIMS <- sf::as_Spatial(sf::st_as_sf(lterNetworkSitesCoords, wkt = 'coordinates'))
  country <- raster::getData(country = countryCode, level = 0)
  country <- rgeos::gSimplify(country, tol = 0.01, topologyPreserve = TRUE)
  mapOfSites <- tmap::tm_shape(country) +
    tmap::tm_borders("grey75", lwd = 1) +
    tmap::tm_shape(lterSitesNetworkPointDEIMS) +
    tmap::tm_dots(col = NA, size = 0.1, shape = 16, title = NA, legend.show = FALSE)
  mapOfSites
}

