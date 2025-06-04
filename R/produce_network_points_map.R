#' Provide a map (image) of sites in an eLTER Network.
#' @description `r lifecycle::badge("stable")`
#' Return a image map object of all of the eLTER sites belonging
#' to an eLTER Network (e.g.
#' \href{https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3}{LTER
#' Italy network}), as a stored into \href{https://deims.org}{DEIMS-SDR}.
#' @param networkDEIMSID A `character`. The DEIMS ID of the network
#' from DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here} and Complete list of ILTER
#' networks \href{https://deims.org/search?f[0]=result_type:network}{here}.
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
#' @importFrom httr2 request req_method req_headers req_retry req_perform
#' @importFrom httr2 resp_check_status resp_body_string
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select
#' @importFrom sf st_as_sf st_crs st_is_valid st_simplify st_bbox
#' @importFrom tibble as_tibble
#' @importFrom stringr str_replace
#' @importFrom ggplot2 ggplot geom_sf coord_sf theme_minimal labs
#' @seealso [geodata::gadm()]
#' @references
#'   \insertRef{httr2}{ReLTER}
#'   
#'   \insertRef{jsonliteR}{ReLTER}
#'
#'   \insertRef{dplyrR}{ReLTER}
#'
#'   \insertRef{sfR}{ReLTER}
#'
#'   \insertRef{tibbleR}{ReLTER}
#'   
#'   \insertRef{stringrR}{ReLTER}
#'   
#'   \insertRef{ggplot2R}{ReLTER}
#'   
#'   \insertRef{geodataR}{ReLTER}
#' @export
#' @examples
#' \dontrun{
#' # Italian sites
#' map <- produce_network_points_map(
#'   networkDEIMSID =
#'     "https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3",
#'   countryCode = "ITA"
#' )
#'
#' # German sites
#' map_LTERGermanSites <- produce_network_points_map(
#'   networkDEIMSID =
#'     "https://deims.org/networks/e904354a-f3a0-40ce-a9b5-61741f66c824",
#'   countryCode = "DEU"
#' )
#' 
#' # Remove scale bar and a north arrow
#' map_LTERGermanSites <- map_LTERGermanSites +
#'   ggplot2::theme(
#'     panel.grid = ggplot2::element_blank(),
#'     axis.text = ggplot2::element_blank(),
#'     axis.ticks = ggplot2::element_blank(),
#'     axis.title = ggplot2::element_blank()
#'   )
#'  map_LTERGermanSites
#'  
#'  # Add annotation scale and North arrow
#'  map_LTERGermanSites <- map_LTERGermanSites +
#'    ggspatial::annotation_scale(
#'      location = "br", # bottom right
#'      width_hint = 0.2) +
#'    ggspatial::annotation_north_arrow(
#'      location = "bl", # bottom right
#'      which_north = "true",
#'      style = north_arrow_fancy_orienteering(),
#'      height = ggplot2::unit(1, "cm"),
#'      width = ggplot2::unit(1, "cm")
#'    )
#'  map_LTERGermanSites
#' }
#'
#' @section The function output:
#' \figure{produce_network_points_map_fig.png}{Map of LTER-D Germany sites}
#'
### function produce_network_points_map
produce_network_points_map <- function(networkDEIMSID, countryCode) {
  # Check if required packages are installed
  if (!requireNamespace("geodata", quietly = TRUE)) {
    stop(
      "\n----\nThe function 'produce_network_points_map()' requires the optional package 'geodata'.\n",
      "Please install it with: install.packages(\"geodata\")\n----\n"
    )
  }
  deimsbaseurl <- get_deims_base_url()
  url <- paste0(
    deimsbaseurl,
    "api/sites?network=",
    sub("^.+/", "", networkDEIMSID)
  )
  export <- httr2::request(base_url = url) %>%
    httr2::req_method("GET") %>%
    httr2::req_headers(Accept = "application/json") %>%
    httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
    httr2::req_perform()
  httr2::resp_check_status(export)
  lterNetworkSitesCoords <- jsonlite::fromJSON(
    httr2::resp_body_string(export) # already UTF-8 encoded
  )
  # check if the response is empty
  if (length(lterNetworkSitesCoords) != 0) {
    lterNetworkSitesCoords$uri <- paste0(
      lterNetworkSitesCoords$id$prefix,
      lterNetworkSitesCoords$id$suffix
    )
    lterNetworkSitesCoords <- lterNetworkSitesCoords %>%
      dplyr::select("title", "uri", "changed", "coordinates")
    # convert coordinates to WKT
    networkSitesGeo <- sf::st_as_sf(
      tibble::as_tibble(lterNetworkSitesCoords),
      wkt = "coordinates"
    )
    sf::st_crs(networkSitesGeo) <- 4326
    networkSitesGeo_valid <- sf::st_is_valid(
      networkSitesGeo
    )
    gadm_fx <- getExportedValue("geodata", "gadm")
    if (any(networkSitesGeo_valid)) {
      if (countryCode %in% isoCodes$Alpha_3 == TRUE) {
        try({
          country <- gadm_fx(country = countryCode, level = 0, path = tempdir())
        })
        countryMap <- if (exists("country")) {
          sf::st_as_sf(country) %>%
            sf::st_simplify(dTolerance = 1000)
        } else {
          NULL
        }
        # Info about the network
        networkPage <- stringr::str_replace(networkDEIMSID, "networks", "api/networks")
        networkTitle <- jsonlite::fromJSON(networkPage)$title
        # produce the map
        mapOfSites <- ggplot2::ggplot() +
          ggplot2::geom_sf(
            data = countryMap, 
            fill = "#FFFFFF", 
            color = "#444444", 
            linewidth = 0.4, 
            alpha = 0.3
          ) +
          ggplot2::geom_sf(
            data = networkSitesGeo, 
            color = "#000000", 
            size = 1
          ) +
          ggplot2::coord_sf(
            xlim = c(sf::st_bbox(countryMap)$xmin, sf::st_bbox(countryMap)$xmax),
            ylim = c(sf::st_bbox(countryMap)$ymin, sf::st_bbox(countryMap)$ymax),
            expand = FALSE
          ) +
          ggplot2::theme_minimal() +
          ggplot2::labs(
            title = networkTitle,
            subtitle = networkDEIMSID
          )
        print(mapOfSites)
      } else {
          message("\n----\nThe map of country cannot be created.
  Please check again the Country code.
  Compare the code provided with the list of code in
  https://en.wikipedia.org/wiki/ISO_3166\n----\n")
        print(mapOfSites)
        }
      } else {
        message("\n----\nThe maps cannot be created because coordinates,
  provided in DEIMS-SDR, have invalid geometry.
  Please check the content and refers this error to DEIMS-SDR contact person
  of the network, citing the Network.iD.\n----\n")
      }
    } else {
      message("\n----\nThe requested page could not be found.
  Please check again the Network.iD\n----\n")
      mapOfSites <- NULL
    }
}
