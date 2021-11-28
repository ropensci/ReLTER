#' @title eLTER produce_site_map function
#' @description This function produces a `map` of the site boundaries
#' as provided by the DEIMS-SDR, within a given country and network.
#' @param deimsid a `character`. The DEIMS ID of network from
#' DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @param countryCode a `character` following the SO 3166-1 alpha-3 codes.
#' This ISO convention consists of three-letter country codes
#' as defined in ISO 3166-1, part of the ISO 3166 standard published by the
#' International Organization for Standardization (ISO), to represent
#' #' countries, dependent territories, and special areas of geographical
#' interest.
#' The map produced by this function will be limited to the country
#' indicated in this parameter; if the network has a extraterritorial sites
#' those will not represented.
#' @param listOfSites a `sf`. List of sites of specific network. This list
#' is needed for showing another points on the map.
#' @param gridNx a `double`. A numeric vector or unit object specifying
#' x-location of viewports about country provided by countryCode parameter.
#' @param gridNy a `double`. A numeric vector or unit object specifying
#' y-location of viewports about country provided by countryCode parameter.
#' @param width a `double`. A numeric vector or unit object specifying width
#' of viewports about country provided by countryCode parameter. Default 0.25.
#' @param height a `double`. A numeric vector or unit object specifying height
#' of viewports about country provided by countryCode parameter. Default 0.25.
#' @param bboxXMin a `double`. A numeric for add some unit of a bbox provided
#' by centroid of the site. Default 0.
#' @param bboxYMin a `double`. A numeric for add some unit of a bbox provided
#' by centroid of the site. Default 0.
#' @param bboxXMax a `double`. A numeric for add some unit of a bbox provided
#' by centroid of the site. Default 0.
#' @param bboxYMax a `double`. A numeric for add some unit of a bbox provided
#' by centroid of the site. Default 0.
#' @return The output of the function is a distribution `image`.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom sf as_Spatial st_as_sfc st_bbox st_crs
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tribble
#' @importFrom raster getData reclassify
#' @importFrom rgeos gSimplify
#' @importFrom rosm osm.raster
#' @importFrom tmap tm_shape tm_rgb tm_dots tm_compass tm_scale_bar tm_layout
#' @importFrom tmap tm_credits tm_basemap tm_borders tm_fill tm_lines
#' @importFrom grid viewport
#' @inportFrom ISOcodes ISO_3166_1
#' @export
#' @examples
#' \dontrun{
#' # Example of Lange Bramke site
#' sitesNetwork <- get_network_sites(
#'   networkDEIMSID =
#'   "https://deims.org/networks/e904354a-f3a0-40ce-a9b5-61741f66c824"
#' )
#' map <- produce_site_map(
#'   deimsid = "https://deims.org/8e24d4f8-d6f6-4463-83e9-73cac2fd3f38",
#'   countryCode = "DEU",
#'   listOfSites = sitesNetwork,
#'   gridNx = 0.2,
#'   gridNy = 0.7
#' )
#'
#' # Example of Eisenwurzen site
#' sitesNetwork <- get_network_sites(
#'   networkDEIMSID =
#'   "https://deims.org/networks/d45c2690-dbef-4dbc-a742-26ea846edf28"
#' )
#' map <- produce_site_map(
#'   deimsid = "https://deims.org/d0a8da18-0881-4ebe-bccf-bc4cb4e25701",
#'   countryCode = "AUT",
#'   listOfSites = sitesNetwork,
#'   gridNx = 0.2,
#'   gridNy = 0.7
#' )
#'
#' # Example of Lake Maggiore site
#' sitesNetwork <- get_network_sites(
#'   networkDEIMSID =
#'   "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
#' )
#' # In the case of Italian sites are selected only true sites and exclused the
#' macrosites.
#' sitesNetwork <- (sitesNetwork[!grepl('^IT', sitesNetwork$title),])
#' map <- produce_site_map(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
#'   countryCode = "ITA",
#'   listOfSites = sitesNetwork,
#'   gridNx = 0.7,
#'   gridNy = 0.35
#' )
#' }
#'
### function produce_site_map
produce_site_map <-
  function(deimsid,
           countryCode,
           listOfSites,
           gridNx,
           gridNy,
           width = 0.25,
           height = 0.25,
           bboxXMin = 0,
           bboxXMax = 0,
           bboxYMin = 0,
           bboxYMax = 0) {
    deimsidExa <- sub("^.+/", "", deimsid)
    siteSelected <- sf::as_Spatial(
      sf::st_as_sfc(
        jsonlite::fromJSON(paste0(
          "https://deims.org/",
          "api/sites/",
          deimsidExa
        ))$attributes$geographic$coordinates,
        crs = "+proj=longlat +datum=WGS84 +no_defs"
      )
    )
    biomeColor <- tibble::tribble(
      ~ geoBonBiome,
      ~ fill,
      ~ border,
      "Marine",
      "#055ca8",
      "#057ae1",
      "Coastal",
      "#43903f",
      "#5ecc58",
      "Fresh water lakes",
      "#03a3b8",
      "#04d0eb",
      "Fresh water rivers",
      "#03a3b8",
      "#04d0eb",
      "Terrestrial",
      "#b07c03",
      "#e8a303"
    )
    geoBonBiome <- jsonlite::fromJSON(
      paste0("https://deims.org/",
             "api/sites/",
             deimsidExa)
      )$attributes$environmentalCharacteristics$geoBonBiome
    color <- biomeColor$fill[biomeColor$geoBonBiome == geoBonBiome[[1]]]
    colorBorder <-
      biomeColor$border[biomeColor$geoBonBiome == geoBonBiome[[1]]]
    geoBoundaries <- jsonlite::fromJSON(
      paste0("https://deims.org/",
             "api/sites/",
             deimsidExa)
      )$attributes$geographic$boundaries
    if (countryCode %in% ISOcodes::ISO_3166_1$Alpha_3 == TRUE) {
      country <- raster::getData(
        country = countryCode,
        level = 0
      )
      country <-
        rgeos::gSimplify(country, tol = 0.01, topologyPreserve = TRUE)
      if (is.null(geoBoundaries)) {
        lterCoords <- siteSelected
        lterSitesFeaturePointDEIMS <-
          sf::as_Spatial(
            sf::st_as_sfc(
              lterCoords,
              crs = "+proj=longlat +datum=WGS84 +no_defs"
            ),
          )
        baseMap <-
          rosm::osm.raster(lterSitesFeaturePointDEIMS, zoomin = -8)
        newBaseMap <- raster::reclassify(baseMap, cbind(NA, 255))
        mapOfSite <-
          tmap::tm_shape(
            newBaseMap,
            raster.downsample = TRUE
          ) +
          tmap::tm_rgb() +
          tmap::tm_shape(lterSitesFeaturePointDEIMS) +
          tmap::tm_dots(
            size = 1,
            shape = 16,
            col = color,
            title = NA,
            legend.show = TRUE
          ) +
          tmap::tm_compass(type = "8star",
                           position = c("right", "bottom")) +
          tmap::tm_scale_bar(position = c("right", "bottom")) +
          tmap::tm_layout(
            main.title = paste0(jsonlite::fromJSON(
              paste0("https://deims.org/",
                     "api/sites/",
                     deimsidExa)
            )$title,
            "\n",
            deimsid),
            main.title.position = "center",
            main.title.color = "black",
            main.title.fontfamily = "sans",
            main.title.size = 0.6,
            legend.bg.color = "white",
            legend.position = c(0.75, 0.9),
            legend.width = -0.24
          ) +
          tmap::tm_credits(
            "Leaflet | &copy; OpenStreetMap contributors -
                       https://www.openstreetmap.org/",
            size = 0.3,
            fontfamily = "sans",
            position = c("left", "bottom")
          ) +
          tmap::tm_basemap(leaflet::providers$Stamen.Watercolor)
        mapOfCentroids <- tmap::tm_shape(country) +
          tmap::tm_borders("grey75", lwd = 1) +
          tmap::tm_shape(listOfSites) +
          tmap::tm_dots(
            col = NA,
            size = 0.01,
            shape = 16,
            title = NA,
            legend.show = FALSE
          ) +
          tmap::tm_shape(siteSelected) +
          tmap::tm_dots(
            col = color,
            size = 0.1,
            shape = 16,
            title = NA,
            legend.show = FALSE
          )
        mapOfSite
        print(mapOfCentroids,
              vp = grid::viewport(gridNx,
                                  gridNy,
                                  width = width,
                                  height = height))
      } else {
        geoBoundaries_sf <- sf::st_as_sfc(geoBoundaries)
        sf::st_crs(geoBoundaries_sf) <- "+proj=longlat +datum=WGS84 +no_defs"
        lterSitesFeatureDEIMS <-
          sf::as_Spatial(geoBoundaries_sf, )
        bboxlterItalySitesFeature <- sf::st_bbox(lterSitesFeatureDEIMS)
        bboxlterItalySitesFeature[1] <-
          sf::st_bbox(lterSitesFeatureDEIMS)[1] +
          bboxXMin
        bboxlterItalySitesFeature[3] <-
          sf::st_bbox(lterSitesFeatureDEIMS)[3] +
          bboxXMax
        bboxlterItalySitesFeature[2] <-
          sf::st_bbox(lterSitesFeatureDEIMS)[2] +
          bboxYMin
        bboxlterItalySitesFeature[4] <-
          sf::st_bbox(lterSitesFeatureDEIMS)[4] +
          bboxYMax
        baseMap <- rosm::osm.raster(bboxlterItalySitesFeature)
        newBaseMap <- raster::reclassify(baseMap, cbind(NA, 255))
        mapOfSite <-
          tmap::tm_shape(newBaseMap) +
          tmap::tm_rgb() +
          tmap::tm_shape(lterSitesFeatureDEIMS) +
          if (class(lterSitesFeatureDEIMS)[1] == "SpatialLines") {
            tmap::tm_lines(col = color)
          } else {
            tmap::tm_borders(col = colorBorder) +
              tmap::tm_fill(col = color, alpha = 0.5)
          } +
          tmap::tm_compass(type = "8star",
                           position = c("right", "bottom")) +
          tmap::tm_scale_bar(position = c("right", "bottom")) +
          tmap::tm_layout(
            main.title = paste0(
              jsonlite::fromJSON(paste0(
                "https://deims.org/",
                "api/sites/",
                substring(deimsid,
                          19)
              ))$title,
              "\nDEIMS.iD ",
              deimsid
            ),
            main.title.position = "center",
            main.title.color = "black",
            main.title.fontfamily = "sans",
            main.title.size = 0.7,
            legend.bg.color = "white",
            legend.position = c(0.75, 0.9),
            legend.width = -0.24
          ) +
          tmap::tm_credits(
            "Leaflet | &copy; OpenStreetMap contributors -
                       https://www.openstreetmap.org/",
            size = 0.5,
            fontfamily = "sans",
            position = c("left", "bottom")
          ) +
          tmap::tm_basemap(leaflet::providers$Stamen.Watercolor)
        print(mapOfSite)
        mapOfCentroids <- tmap::tm_shape(country) +
          tmap::tm_borders("grey75", lwd = 1) +
          tmap::tm_shape(listOfSites) +
          tmap::tm_dots(
            col = NA,
            size = 0.01,
            shape = 16,
            title = NA,
            legend.show = FALSE
          ) +
          tmap::tm_shape(siteSelected) +
          tmap::tm_dots(
            col = color,
            size = 0.1,
            shape = 16,
            title = NA,
            legend.show = FALSE
          )
        print(mapOfCentroids,
              vp = grid::viewport(
                gridNx,
                gridNy,
                width = width,
                height = height
              )
            )
      }
    } else {
      message(
        "\n----\nThe map of site cannot be made properly.
Please check again the Country code.
Compare the code provided with the list of code in Wikipage
https://en.wikipedia.org/wiki/ISO_3166\n----\n"
      )
      mapOfCentroids <- NULL
    }
  }
