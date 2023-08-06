#' Retrieve a list of sites in an eLTER Network.
#' @description `r lifecycle::badge("stable")`
#' This function return a spatial point vector object including
#' title, date late updated, URI, and coordinates, stored in
#' \href{https://deims.org/}{DEIMS-SDR catalogue}, of all the eLTER sites
#' belonging to an eLTER Network (e.g.
#' \href{https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3}{LTER-
#' Italy network}).
#' @param networkDEIMSID A `character`. The DEIMS ID of the network from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here} and Complete list of
#' networks \href{https://deims.org/search?f[0]=result_type:network}{here}.
#' The DEIMS ID of network is the URL for the network page.
#' @return The output of the function is a point vector of `sf` class
#' (package sf) of the network's sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_as_sf st_is_valid st_cast
#' @importFrom dplyr select mutate as_tibble add_row
#' @importFrom leaflet leaflet addTiles addMarkers
#' @importFrom httr RETRY content
#' @importFrom Rdpack reprompt
#' @references
#'   \insertRef{httrR}{ReLTER}
#'
#'   \insertRef{dplyrR}{ReLTER}
#'
#'   \insertRef{jsonliteR}{ReLTER}
#'
#'   \insertRef{sfR}{ReLTER}
#'
#'   \insertRef{leafletR}{ReLTER}
#' @export
#' @examples
#' \dontrun{
#' # The sites of LTER-Italy network
#' listSites <- get_network_sites(
#'   networkDEIMSID =
#'     "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
#' )
#' listSites
#'
#' # The sites of LTER Europe network
#' euSites <- get_network_sites(
#'   networkDEIMSID =
#'     "https://deims.org/networks/4742ffca-65ac-4aae-815f-83738500a1fc"
#' )
#' euSites
#' }
#'
### function get_network_sites
get_network_sites <- function(networkDEIMSID) {
  deimsbaseurl <- get_deims_base_url()
  url <- paste0(deimsbaseurl,
                "api/sites?network=",
                sub("^.+/", "", networkDEIMSID))
  export <- httr::RETRY("GET", url = url, times = 5)
  lterNetworkSitesCoords <- jsonlite::fromJSON(
    httr::content(export, as = "text", encoding = "UTF-8"))
  lterNetworkSitesCoords <- dplyr::as_tibble(lterNetworkSitesCoords)
  if (length(lterNetworkSitesCoords) != 0) {
    # check if some site has MULTIPOINTS instead POINT and convert it
    for (i in seq_len(nrow(lterNetworkSitesCoords))) {
      
      geom_new_points_sf <- sf::st_sfc()
      class(geom_new_points_sf)[1] <- "sfc_POINT" # for points
      new_points_sf <- sf::st_sf(
        title = character(0),
        changed = character(0),
        uri = character(0),
        coordinates = geom_new_points_sf,
        crs = 4326
      )
      
      if (grepl("MULTIPOINT", lterNetworkSitesCoords$coordinates[i])) {
        multipoint_site_sf <- sf::st_as_sf(
          lterNetworkSitesCoords[i, ],
          wkt = "coordinates",
          crs = 4326
        )
        points_site_sf <- sf::st_cast(x = multipoint_site_sf, to = "POINT") %>%
          dplyr::mutate(
            uri = paste0(
              id$prefix,
              id$suffix
            )
          ) %>%
          dplyr::select(
            "title", "changed", "uri", "coordinates"
          )
        new_points_sf <- new_points_sf %>%
          dplyr::add_row(points_site_sf)
        lterNetworkSitesCoords <- lterNetworkSitesCoords[-i, ]
      }
    }
    # transform to sf
    lterSitesNetworkPointDEIMS <- sf::st_as_sf(
      lterNetworkSitesCoords,
      wkt = "coordinates",
      crs = 4326
    )
    lterSitesNetworkPointDEIMS$uri <- paste0(
      lterSitesNetworkPointDEIMS$id$prefix,
      lterSitesNetworkPointDEIMS$id$suffix
    )
    lterSitesNetworkPointDEIMS <- lterSitesNetworkPointDEIMS %>%
      dplyr::select(
        "title", "changed", "uri", "coordinates"
      )
    lterSitesNetworkPointDEIMS <- lterSitesNetworkPointDEIMS %>%
      dplyr::add_row(new_points_sf)
    lSNPD_valid <- sf::st_is_valid(
      lterSitesNetworkPointDEIMS
    )
    # harmonization of date and time
    lterSitesNetworkPointDEIMS$changed <- lubridate::as_datetime(lterSitesNetworkPointDEIMS$changed)
    # checking MULTIPOINT geometry
    lSNPD_type <- sf::st_geometry_type(
      x = lterSitesNetworkPointDEIMS,
      by_geometry = TRUE
    )
    suppressWarnings(
    if (isTRUE(any(match("MULTIPOINT", lSNPD_type)) != "POINT")) {
      lSNPD_temp <- NULL
      for (i in match("MULTIPOINT", lSNPD_type)) {
        lSNPD_splitted <- sf::st_cast(
          x = lterSitesNetworkPointDEIMS[c(i), ],
          to = "POINT",
          do_split = TRUE
        )
        lSNPD_temp <- rbind(
          lSNPD_temp,
          lSNPD_splitted
        )
        lterSitesNetworkPointDEIMS <- lterSitesNetworkPointDEIMS[c(-i), ]
      }
      lterSitesNetworkPointDEIMS <- rbind(
        lterSitesNetworkPointDEIMS,
        lSNPD_temp
      )
    })
    # end checking

    if (any(lSNPD_valid)) {
      map <- leaflet::leaflet(lterSitesNetworkPointDEIMS) %>%
        leaflet::addTiles() %>%
        leaflet::addMarkers(
          clusterOptions = leaflet::markerClusterOptions(),
          popup = paste0(
            "<b>Site name: </b>"
            , lterSitesNetworkPointDEIMS$title
            , "<br>"
            , "<a href='"
            , lterSitesNetworkPointDEIMS$uri
            , "' target='_blank'>"
            , "Click Here to View site landing page</a>"
          )
        )
      print(map)
      message("\n----\nThe number of the sites on the map can be more than
in the network, since some are represented in DEIMS-SDR by multiple points.
(e.g. https://deims.org/18998d9a-7ff5-4e9d-a971-9694e0a4914d).\n----\n")
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
