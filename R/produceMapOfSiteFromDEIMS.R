#' @title eLTER produceMapOfSiteFromDEIMS function
#' @description This function provide a `map` of the site boundaries provided by site manager in the DEIMS-SDR.
#' @param networkDEIMSID A `character`. It is the DEIMS iD of network make from DEIMS-SDR website. More information about DEIMS iD in this \href{https://deims.org/docs/deimsid.html}{page}, and at this \href{https://deims.org/search?f%5B0%5D=result_type%3Anetwork}{page} the complete list of ILTER networks.
#' @return The output of the function is a distribution `image`.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import jsonlite sf sp tibble rgeos rosm raster tmap grid
#' @export
#' @examples
#' sitesNetwork <- getNetworkSites(networkDEIMSID = 'https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3')
#' sitesNetwork <- (sitesNetwork[!grepl('^IT', sitesNetwork$title),])
#' map <- produceMapOfSiteFromDEIMS(
#'   deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe',
#'   countryCode = 'ITA',
#'   listOfSites = sitesNetwork,
#'   gridNx = 0.7,
#'   gridNy = 0.35,
#'   width = 0.25,
#'   height = 0.25,
#'   siteName = 'Lago Maggiore',
#'   bboxXMax = 0,
#'   bboxXMin = 0,
#'   bboxYMin = 0,
#'   bboxYMax = 0
#' )
#'
### function produceMapOfSiteFromDEIMS
produceMapOfSiteFromDEIMS <- function(deimsid, countryCode, listOfSites, gridNx, gridNy, width, height, siteName, bboxXMin, bboxXMax, bboxYMin, bboxYMax) {
  siteSelected <- sf::as_Spatial(sf::st_as_sfc(jsonlite::fromJSON(paste0("https://deims.org/", "api/sites/", substring(deimsid, 19)))$attributes$geographic$coordinates),)
  siteSelected@proj4string <- sp::CRS('+proj=longlat +datum=WGS84 +no_defs')

  biomeColor <- tibble::tribble(
    ~geoBonBiome, ~fill, ~border,
    "marine", "#055ca8", "#057ae1",
    "coastal", "#43903f", "#5ecc58",
    "fresh_water_lakes", "#03a3b8", "#04d0eb",
    "fresh_water_rivers", "#03a3b8", "#04d0eb",
    "terrestrial", "#b07c03", "#e8a303"
  )

  geoBonBiome <- jsonlite::fromJSON(paste0("https://deims.org/", "api/sites/", substring(deimsid, 19)))$attributes$environmentalCharacteristics$geoBonBiome
  color <- biomeColor$fill[biomeColor$geoBonBiome == geoBonBiome]
  colorBorder <- biomeColor$border[biomeColor$geoBonBiome == geoBonBiome]
  geoBoundaries <- jsonlite::fromJSON(paste0("https://deims.org/", "api/sites/", substring(deimsid, 19)))$attributes$geographic$boundaries

  country <- raster::getData(country = countryCode, level = 0)
  country <- rgeos::gSimplify(country, tol = 0.01, topologyPreserve = TRUE)

  if (is.null(geoBoundaries)) {
    lterCoords <- siteSelected
    lterSitesFeaturePointDEIMS <- sf::as_Spatial(sf::st_as_sfc(lterCoords),)
    lterSitesFeaturePointDEIMS@proj4string <- sp::CRS('+proj=longlat +datum=WGS84 +no_defs')
    baseMap <- rosm::osm.raster(lterSitesFeaturePointDEIMS, zoomin = -8)
    # plot(baseMap)
    newBaseMap <- raster::reclassify(baseMap, cbind(NA, 255))
    # plot(newBaseMap)
    mapOfSite <-
      tmap::tm_shape(newBaseMap, ) + tmap::tm_rgb() +
      tmap::tm_shape(lterSitesFeaturePointDEIMS) +
      tmap::tm_dots(size = 1, shape = 16, col = color, title = NA, legend.show = TRUE) +
      # tmap::tm_text(text = "long_name", col = "black", fontfamily = "sans", size = 1.5) +
      tmap::tm_compass(type = "8star", position = c("right", "bottom")) +
      tmap::tm_scale_bar(position = c("right", "bottom")) +
      tmap::tm_layout(main.title = paste0(jsonlite::fromJSON(paste0("https://deims.org/", "api/sites/", substring(deimsid, 19)))$title, "\n", deimsid),
                      main.title.position = "center",
                      main.title.color = "black",
                      main.title.fontfamily = "sans",
                      main.title.size = 0.6,
                      legend.bg.color = "white",
                      legend.position = c(0.75, 0.9),
                      legend.width = -0.24
      ) +
      # tmap::tm_credits(paste0("DEIMS ID: ", centroidsFeatureSPItalySelected$DEIMS_ID),
      #                  size = 0.7,
      #                  fontfamily = "sans",
      #                  position = c("left", "top")) +
      tmap::tm_credits("Leaflet | © OpenStreetMap contributors - https://www.openstreetmap.org/",
                       size = 0.3,
                       fontfamily = "sans",
                       position = c("left", "bottom")) +
      tmap::tm_basemap(leaflet::providers$Stamen.Watercolor)

    mapOfCentroids <- tmap::tm_shape(country) +
      tmap::tm_borders("grey75", lwd = 1) +
      tmap::tm_shape(listOfSites) +
      tmap::tm_dots(col = NA, size = 0.01, shape = 16, title = NA, legend.show = FALSE) +
      tmap::tm_shape(siteSelected) +
      tmap::tm_dots(col = color, size = 0.1, shape = 16, title = NA, legend.show = FALSE)

    mapOfSite
    print(mapOfCentroids, vp = grid::viewport(gridNx, gridNy, width = width, height = height))
    # tmap::tmap_save(
    #   tm = mapOfSite,
    #   filename = paste0("images/", paste0(gsub("[[:punct:]]", "_", siteName)), "_test.png"),
    #   dpi = 400,
    #   insets_tm = mapOfCentroids,
    #   insets_vp = grid::viewport(gridNx, gridNy, width = 0.25, height = 0.25)
    # )
  } else {
    lterSitesFeatureDEIMS <- sf::as_Spatial(sf::st_as_sfc(geoBoundaries),)
    lterSitesFeatureDEIMS@proj4string <- sp::CRS('+proj=longlat +datum=WGS84 +no_defs')
    bboxlterItalySitesFeature <- sp::bbox(lterSitesFeatureDEIMS)
    bboxlterItalySitesFeature[1] <- sp::bbox(lterSitesFeatureDEIMS)[1] + bboxXMin
    bboxlterItalySitesFeature[3] <- sp::bbox(lterSitesFeatureDEIMS)[3] + bboxXMax
    bboxlterItalySitesFeature[2] <- sp::bbox(lterSitesFeatureDEIMS)[2] + bboxYMin
    bboxlterItalySitesFeature[4] <- sp::bbox(lterSitesFeatureDEIMS)[4] + bboxYMax
    baseMap <- rosm::osm.raster(bboxlterItalySitesFeature)
    # plot(baseMap)
    newBaseMap <- raster::reclassify(baseMap, cbind(NA, 255))
    # plot(newBaseMap)
    mapOfSite <-
      tmap::tm_shape(newBaseMap, ) + tmap::tm_rgb() +
      tmap::tm_shape(lterSitesFeatureDEIMS) +
      if (class(lterSitesFeatureDEIMS)[1] == 'SpatialLines') {
        tmap::tm_lines(col = color)
      } else {
        tmap::tm_borders(col = colorBorder) +
        tmap::tm_fill(col = color, alpha = 0.5)
      } +
      # tmap::tm_text(text = "long_name", col = "black", fontfamily = "sans", size = 1.5) +
      tmap::tm_compass(type = "8star", position = c("right", "bottom")) +
      tmap::tm_scale_bar(position = c("right", "bottom")) +
      tmap::tm_layout(main.title = paste0(
        jsonlite::fromJSON(
          paste0("https://deims.org/",
                 "api/sites/",
                 substring(
                   deimsid,
                   19
                 )
          )
        )$title,
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
      # tmap::tm_credits(paste0("DEIMS ID: ", centroidsFeatureSPItalySelected$DEIMS_ID),
      #                  size = 0.7,
      #                  fontfamily = "sans",
      #                  position = c("left", "top")) +
      tmap::tm_credits("Leaflet | © OpenStreetMap contributors - https://www.openstreetmap.org/",
                       size = 0.5,
                       fontfamily = "sans",
                       position = c("left", "bottom")) +
      tmap::tm_basemap(leaflet::providers$Stamen.Watercolor)

    mapOfCentroids <- tmap::tm_shape(country) +
      tmap::tm_borders("grey75", lwd = 1) +
      tmap::tm_shape(listOfSites) +
      tmap::tm_dots(col = NA, size = 0.01, shape = 16, title = NA, legend.show = FALSE) +
      tmap::tm_shape(siteSelected) +
      tmap::tm_dots(col = color, size = 0.1, shape = 16, title = NA, legend.show = FALSE)

    print(mapOfSite)
    print(mapOfCentroids, vp = grid::viewport(gridNx, gridNy, width = width, height = height))
    # tmap::tmap_save(
    #   tm = mapOfSite,
    #   filename = paste0("images/", paste0(gsub("[[:punct:]]", "_", siteName)), ".png"),
    #   dpi = 400,
    #   insets_tm = mapOfCentroids,
    #   insets_vp = grid::viewport(gridNx, gridNy, width = 0.25, height = 0.25)
    # )
  }
}
