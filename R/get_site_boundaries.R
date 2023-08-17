#' eLTER get_site_boundaries function
#' @description `r lifecycle::badge("stable")`
#' This internal function retrieves the boundary of a specified eLTER site
#' and a view of the site boundaries on a leaflet map is shown.
#' All the info are taken from the DEIMS-SDR sites API.
#' If the boundary is missing, a warning message is printed in the R console.
#' @param deimsid A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @param show_map A `boolean`. When TRUE the boundary will be plotted on a
#' Leaflet map. Default FALSE.
#' @param with_locations A `boolean`. When TRUE all site boundaries and
#' related locations are returned (Sampling Location or Equipment Location).
#' Default FALSE.
#' @return The output of the function is an `sf` object, the boundary of the
#' site or NA if the boundary is missing from DEIMS-SDR. In addition, as
#' `html map` with boundaries of the site is plotted.
#' If the parameter `with_locations` is TRUE return a list woth the boundary
#' and all related locations declared in DEIMS-SDR. A map with these is printed
#' if `show_map` is TRUE.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @author  Micha Silver, phD (2021) \email{silverm@@post.bgu.ac.il}
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @importFrom dplyr mutate select as_tibble
#' @importFrom geojsonsf geojson_sf
#' @importFrom leaflet leaflet addTiles addPolygons
#' @importFrom leaflet addCircleMarkers
#' @importFrom sf st_sf st_sfc st_geometry_type
#' @importFrom utils capture.output
#' @export
#' @examples
#' # LTER Zöbelboden
#' boundaries <- get_site_boundaries(
#'   deimsid = "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",
#'   show_map = TRUE,
#'   with_locations = TRUE
#' )
#' boundaries
#' 
#' @section The function output:
#' \figure{get_site_boundaries_fig.png}{Map of "LTER Zöbelboden, Austria" 
#' with locations}
#'
### function get_site_boundaries
get_site_boundaries <- function(
    deimsid,
    show_map = FALSE,
    with_locations = FALSE
  ) {
    url.geoserver <- paste0("https://deims.org/geoserver/deims/ows?",
      "service=WFS&version=2.0.0&request=GetFeature&TypeName=deims:deims_sites_boundaries&",
      "outputFormat=application%2Fjson&CQL_FILTER=deimsid=%27", URLencode(deimsid), "%27"
    )
    if (with_locations == FALSE) {
      geoBoundaries <- geojsonsf::geojson_sf(url.geoserver)
      geoBoundaries <- geoBoundaries %>%
        dplyr::mutate(title = name, uri = deimsid, .before = geometry) %>%
        dplyr::select(-c("name", "deimsid", "field_elevation_avg_value"))
      if (show_map == TRUE) {
        map <- leaflet::leaflet(geoBoundaries) %>%
          leaflet::addTiles() %>%
          leaflet::addPolygons(
            data = geoBoundaries,
            color = "white",
            weight = 3,
            opacity = 1,
            fill = TRUE,
            fillColor = "#eaa75e",
            fillOpacity = 0.8,
            popup = paste0(
              "<b>Site title: </b>",
              "<br><a href='",
              geoBoundaries$uri,
              "' target='_blank'>",
              geoBoundaries$title,
              "</a><br>"
            )
          )
        print(map)
        geoBoundaries
      } else {
        geoBoundaries
      }
    } else {
      boundaries <- list(
        site = "",
        locations = sf::st_sf(
          sf::st_sfc(),
          crs = 4326
        )
      )
      boundaries$site <- get_site_info(
        deimsid = deimsid,
        category = "Boundaries"
      )
      qo <- queries_jq[[get_deims_API_version()]]$site_boundaries 
      jj <- get_id(deimsid, qo$path)
      if (is.na(attr(jj, "status"))) {
        invisible(
          utils::capture.output(
            boundariesInfo <- dplyr::as_tibble(do_Q(qo$query, jj))
          )
        )
      }
      relatedLocations <- boundariesInfo$relatedLocations[[1]] %>%
        dplyr::mutate(uri = paste0(
          id$prefix, id$suffix
        ), .after = title) %>%
        dplyr::select(-c(
          "id"
        ))
      map <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(
          data = boundaries$site,
          color = "white",
          weight = 3,
          opacity = 1,
          fill = TRUE,
          fillColor = "#eaa75e",
          fillOpacity = 0.8,
          popup = paste0(
            "<b>Site title: </b>",
            "<br><a href='",
            boundaries$site$uri,
            "' target='_blank'>",
            boundaries$site$title,
            "</a><br>"
          )
        )
      for (i in seq_len(nrow(relatedLocations))) {
        location <- get_location_info(
          locationid = relatedLocations$uri[i]
        )
        if (sf::st_geometry_type(location) == "POINT") {
          if (location$locationType.label == "not declared") {
            map <- map %>%
              leaflet::addCircleMarkers(
                data = location,
                radius = 5,
                color = "white",
                weight = 3,
                opacity = 1,
                fill = TRUE,
                fillColor = "red",
                fillOpacity = 0.6,
                popup = paste0(
                  "<b>Location title: </b>",
                  "<br>",
                  location$title,
                  "<br>",
                  "<b>Location type: </b>",
                  "<br>",
                  location$locationType.label,
                  "<br>",
                  "<b>Related ",
                  location$relatedSite.type,
                  ": </b><br>",
                  "<a href='",
                  location$relatedSite.uri,
                  "' target='_blank'>",
                  location$relatedSite.title,
                  "</a>"
                )
              )
          } else if (location$locationType.label == "Sampling Location") {
            map <- map %>%
              leaflet::addCircleMarkers(
                data = location,
                radius = 8,
                color = "white",
                weight = 3,
                opacity = 1,
                fill = TRUE,
                fillColor = "#336600",
                fillOpacity = 0.6,
                popup = paste0(
                  "<b>Location title: </b>",
                  "<br>",
                  location$title,
                  "<br>",
                  "<b>Location type: </b>",
                  "<br>",
                  location$locationType.label,
                  "<br>",
                  "<b>Related ",
                  location$relatedSite.type,
                  ": </b><br>",
                  "<a href='",
                  location$relatedSite.uri,
                  "' target='_blank'>",
                  location$relatedSite.title,
                  "</a>"
                )
              )
          } else if (location$locationType.label == "Equipment Location") {
            map <- map %>%
              leaflet::addCircleMarkers(
                data = location,
                radius = 8,
                color = "white",
                weight = 3,
                opacity = 1,
                fill = TRUE,
                fillColor = "#1A1AFF",
                fillOpacity = 0.6,
                popup = paste0(
                  "<b>Location title: </b>",
                  "<br>",
                  location$title,
                  "<br>",
                  "<b>Location type: </b>",
                  "<br>",
                  location$locationType.label,
                  "<br>",
                  "<b>Related ",
                  location$relatedSite.type,
                  ": </b><br>",
                  "<a href='",
                  location$relatedSite.uri,
                  "' target='_blank'>",
                  location$relatedSite.title,
                  "</a>"
                )
              )
          }
        } else if (sf::st_geometry_type(location) == "POLYGON") {
          if (location$locationType.label == "not declared") {
            map <- map %>%
              leaflet::addPolygons(
                data = location,
                color = "white",
                weight = 3,
                opacity = 1,
                fill = TRUE,
                fillColor = "red",
                fillOpacity = 0.6,
                popup = paste0(
                  "<b>Location title: </b>",
                  "<br>",
                  location$title,
                  "<br>",
                  "<b>Location type: </b>",
                  "<br>",
                  location$locationType.label,
                  "<br>",
                  "<b>Related ",
                  location$relatedSite.type,
                  ": </b><br>",
                  "<a href='",
                  location$relatedSite.uri,
                  "' target='_blank'>",
                  location$relatedSite.title,
                  "</a>"
                )
              )
          } else if (location$locationType.label == "Sampling Location") {
            map <- map %>%
              leaflet::addPolygons(
                data = location,
                color = "white",
                weight = 3,
                opacity = 1,
                fill = TRUE,
                fillColor = "#336600",
                fillOpacity = 0.6,
                popup = paste0(
                  "<b>Location title: </b>",
                  "<br>",
                  location$title,
                  "<br>",
                  "<b>Location type: </b>",
                  "<br>",
                  location$locationType.label,
                  "<br>",
                  "<b>Related ",
                  location$relatedSite.type,
                  ": </b><br>",
                  "<a href='",
                  location$relatedSite.uri,
                  "' target='_blank'>",
                  location$relatedSite.title,
                  "</a>"
                )
              )
          } else if (location$locationType.label == "Equipment Location") {
            map <- map %>%
              leaflet::addPolygons(
                data = location,
                color = "white",
                weight = 3,
                opacity = 1,
                fill = TRUE,
                fillColor = "#1A1AFF",
                fillOpacity = 0.6,
                popup = paste0(
                  "<b>Location title: </b>",
                  "<br>",
                  location$title,
                  "<br>",
                  "<b>Location type: </b>",
                  "<br>",
                  location$locationType.label,
                  "<br>",
                  "<b>Related ",
                  location$relatedSite.type,
                  ": </b><br>",
                  "<a href='",
                  location$relatedSite.uri,
                  "' target='_blank'>",
                  location$relatedSite.title,
                  "</a>"
                )
              )
          }
        } else if (sf::st_geometry_type(location) == "MULTIPOLYGON") {
          if (location$locationType.label == "not declared") {
            map <- map %>%
              leaflet::addPolygons(
                data = location,
                color = "white",
                weight = 3,
                opacity = 1,
                fill = TRUE,
                fillColor = "red",
                fillOpacity = 0.6,
                popup = paste0(
                  "<b>Location title: </b>",
                  "<br>",
                  location$title,
                  "<br>",
                  "<b>Location type: </b>",
                  "<br>",
                  location$locationType.label,
                  "<br>",
                  "<b>Related ",
                  location$relatedSite.type,
                  ": </b><br>",
                  "<a href='",
                  location$relatedSite.uri,
                  "' target='_blank'>",
                  location$relatedSite.title,
                  "</a>"
                )
              )
          } else if (location$locationType.label == "Sampling Location") {
            map <- map %>%
              leaflet::addPolygons(
                data = location,
                color = "white",
                weight = 3,
                opacity = 1,
                fill = TRUE,
                fillColor = "#336600",
                fillOpacity = 0.6,
                popup = paste0(
                  "<b>Location title: </b>",
                  "<br>",
                  location$title,
                  "<br>",
                  "<b>Location type: </b>",
                  "<br>",
                  location$locationType.label,
                  "<br>",
                  "<b>Related ",
                  location$relatedSite.type,
                  ": </b><br>",
                  "<a href='",
                  location$relatedSite.uri,
                  "' target='_blank'>",
                  location$relatedSite.title,
                  "</a>"
                )
              )
          } else if (location$locationType.label == "Equipment Location") {
            map <- map %>%
              leaflet::addPolygons(
                data = location,
                color = "white",
                weight = 3,
                opacity = 1,
                fill = TRUE,
                fillColor = "#1A1AFF",
                fillOpacity = 0.6,
                popup = paste0(
                  "<b>Location title: </b>",
                  "<br>",
                  location$title,
                  "<br>",
                  "<b>Location type: </b>",
                  "<br>",
                  location$locationType.label,
                  "<br>",
                  "<b>Related ",
                  location$relatedSite.type,
                  ": </b><br>",
                  "<a href='",
                  location$relatedSite.uri,
                  "' target='_blank'>",
                  location$relatedSite.title,
                  "</a>"
                )
              )
          }
        }
        boundaries$locations <- boundaries$locations %>% rbind(location)
      }
      if (show_map == TRUE) {
        print(map)
      }
      boundaries
    }
}
