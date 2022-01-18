#' @title eLTER produce_network_points_map function
#' @description This function provides a map (image) of sites in an LTER network
#' @param networkDEIMSID A `character`. The DEIMS ID of the network
#' from DEIMS-SDR website. More information about DEIMS network ID from:
#' \href{https://deims.org/docs/deimsid.html}{page}, and
#' \href{https://deims.org/search?f[0]=result_type:network}{page}
#' (the complete list of ILTER networks.)
#' @param countryCode A `character` following the ISO 3166-1 alpha-3 codes.
#' This ISO convention consists of three-letter country codes as defined in
#' ISO 3166-1. The ISO 3166 standard published by the International
#' Organization for Standardization (ISO), to represent countries, dependent
#' territories, and special areas of geographical interest. The map produced by
#' this function will be limited only to the country indicated in this
#' parameter, if the network has a extraterritorial sites those will not
#' represented.
#' @return The output of the function is a `tmap` plot containing an image of
#' geographic distribution of the network of sites present in the chosen
#' country.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom sf as_Spatial st_as_sf st_crs st_is_valid
#' @importFrom raster getData
#' @importFrom tmap tm_shape tm_borders tm_dots
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
#' @importFrom httr RETRY content
#' @export
#' @examples
#' \dontrun{
#' # Italian sites
#' map <- produce_network_points_map(
#'   networkDEIMSID =
#'   "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3",
#'   countryCode = "ITA"
#' )
#' map
#'
#' # German sites
#' produce_network_points_map(
#'   networkDEIMSID =
#'   "https://deims.org/networks/e904354a-f3a0-40ce-a9b5-61741f66c824",
#'   countryCode = "DEU"
#' )
#' }
#'
### function produce_network_points_map
produce_network_points_map <- function(networkDEIMSID, countryCode) {
  # suppressWarnings({
    url <- paste0(
      "https://deims.org/",
      "api/sites?network=",
      sub("^.+/", "", networkDEIMSID)
    )
    export <- httr::RETRY("GET", url = url, times = 5)
    lterNetworkSitesCoords <- jsonlite::fromJSON(
      httr::content(export, as = "text", encoding = "UTF-8")
    )
    if (length(lterNetworkSitesCoords) != 0) {
      lterNetworkSitesCoords$uri <- paste0(
        lterNetworkSitesCoords$id$prefix,
        lterNetworkSitesCoords$id$suffix
      )
      lterNetworkSitesCoords <- lterNetworkSitesCoords %>%
        dplyr::select("title", "uri", "changed", "coordinates")
      networkSitesGeo <- sf::st_as_sf(
        tibble::as_tibble(lterNetworkSitesCoords),
        wkt = "coordinates"
      )
      sf::st_crs(networkSitesGeo) <- 4326
      networkSitesGeo_valid <- sf::st_is_valid(
        networkSitesGeo
      )
      if (any(networkSitesGeo_valid)) {
        if (countryCode %in% isoCodes$Alpha_3 == TRUE) {
          country <- raster::getData(country = countryCode, level = 0)
          country <- rgeos::gSimplify(
            country,
            tol = 0.01,
            topologyPreserve = TRUE
          )
          mapOfSites <- tmap::tm_shape(country) +
            tmap::tm_borders("grey75", lwd = 1) +
            tmap::tm_shape(networkSitesGeo) +
            tmap::tm_dots(
              col = NA,
              size = 0.1,
              shape = 16,
              title = NA,
              legend.show = FALSE
            )
          suppressWarnings(print(mapOfSites)) # FIXME manage
          networkSitesGeo
        } else {
          mapOfSites <- tmap::tm_shape(networkSitesGeo) +
            tmap::tm_dots(
              col = NA,
              size = 0.1,
              shape = 16,
              title = NA,
              legend.show = FALSE
            )
          message("\n----\nThe map of site cannot be created.
  Please check again the Country code.
  Compare the code provided with the list of code in
  https://en.wikipedia.org/wiki/ISO_3166\n----\n")
          print(mapOfSites)
          networkSitesGeo
        }
      } else {
        message("\n----\nThe maps cannot be created because coordinates,
  provided in DEIMS-SDR, have invalid geometry.
  Please check the content and refers this error to DEIMS-SDR contact person
  of the network, citing the Network.iD.\n----\n")
        mapOfSites <- tmap::tm_shape(country) +
          tmap::tm_borders("grey75", lwd = 1)
        print(mapOfSites)
        networkSitesGeo
      }
    } else {
      message("\n----\nThe requested page could not be found.
  Please check again the Network.iD\n----\n")
      networkSitesGeo <- NULL
      mapOfSites <- NULL
    }
  # })
}
