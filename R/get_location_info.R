#' Obtain the information about of an eLTER location.
#' @description `r lifecycle::badge("stable")`
#' This function obtains the information about of an eLTER
#' location (e.g.
#' \url{https://deims.org/location/12b38f3f-7e72-425a-80c7-7cad35ce4c7b})
#' provided in \href{https://deims.org/}{DEIMS-SDR catalogue}.
#' @param location_id A `character`. It is the DEIMS ID of location make from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' The DEIMS.iD of activity is the URL for the location page.
#' @param show_map A `boolean`. If TRUE a Leaflet map with occurrences
#' is shown. Default FALSE.
#' @return The output of the function is a `tibble` with main features of
#' the location in a site, and a `leaflet` map plot.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom dplyr as_tibble select mutate
#' @importFrom utils capture.output
#' @importFrom sf st_as_sf st_is_valid st_as_text
#' @importFrom sf st_multipolygon st_polygon st_point
#' @importFrom sf st_geometry_type
#' @importFrom leaflet leaflet addTiles addPolygons
#' @importFrom leaflet addCircleMarkers
#' @importFrom Rdpack reprompt
#' @importFrom lubridate as_datetime
#' @importFrom units set_units
#' @references
#'   \insertRef{dplyrR}{ReLTER}
#'
#'   \insertRef{utilsR}{ReLTER}
#'
#'   \insertRef{sfR}{ReLTER}
#'
#'   \insertRef{leafletR}{ReLTER}
#' @export
#' @examples
#' # Sampling location multipolygon
#' location <- ReLTER::get_location_info(
#'   location_id =
#'     "https://deims.org/location/85dc6019-9654-4ba0-8338-08c4ffe8fe47",
#'   show_map = TRUE
#' )
#' location
#' 
#' # Sampling location polygon
#' location <- ReLTER::get_location_info(
#'   location_id =
#'     "https://deims.org/location/12b38f3f-7e72-425a-80c7-7cad35ce4c7b",
#'   show_map = TRUE
#' )
#' location
#' 
#' # Equipment location polygon
#' location <- ReLTER::get_location_info(
#'   location_id =
#'     "https://deims.org/locations/04de8301-b481-4ed2-89ff-2f48562e2514",
#'   show_map = TRUE
#' )
#' location
#' 
#' # Sampling location point
#' location <- ReLTER::get_location_info(
#'   location_id =
#'     "https://deims.org/location/ec1a58f7-1aee-4e3f-bec3-4eb1516ee905",
#'   show_map = TRUE
#' )
#' location
#' 
#' # Sampling location point with location type null
#' location <- ReLTER::get_location_info(
#'   location_id =
#'     "https://deims.org/location/c3db70c3-5d2c-4905-801c-7b7a5c4d00d9",
#'   show_map = TRUE
#' )
#' location
#'
#' @section The function output:
#' \figure{get_location_info_fig.png}{Map of "LTER Zöbelboden, Austria, 
#' Project area" location}
#'
### function get_location_info
get_location_info <- function(location_id, show_map = FALSE) {
  qo <- queries_jq_deims[[get_deims_API_version()]]$location_info_type
  jj <- get_id(location_id, qo$path)
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        types <- dplyr::as_tibble(do_Q(qo$query, jj))
      )
    )
    geometryType <- types$geometryType
    if (length(types) == 2) {
      types <- types %>%
        dplyr::mutate(
          locationType.label = "not declared",
          locationType.uri = NA,
          .before = locationType
        ) %>%
        dplyr::select(-c(
          "locationType"
        ))
      locationType <- types$locationType.label
      if (geometryType == "Point") {
        qo <- queries_jq_deims[[get_deims_API_version()]]$location_info_point
        jj <- get_id(location_id, qo$path)
        location <- dplyr::as_tibble(do_Q(qo$query, jj)) %>%
          dplyr::mutate(
            locationType.label = "not declared",
            locationType.uri = NA,
            .before = locationType
          ) %>%
          dplyr::select(-c(
            "locationType"
          ))
      } else if (geometryType == "Polygon") {
        qo <- queries_jq_deims[[get_deims_API_version()]]$location_info_polygon
        jj <- get_id(location_id, qo$path)
        location <- dplyr::as_tibble(do_Q(qo$query, jj)) %>%
          dplyr::mutate(
            locationType.label = "not declared",
            locationType.uri = NA,
            .before = locationType
          ) %>%
          dplyr::select(-c(
            "locationType"
          ))
      } else if (geometryType == "MultiPolygon") {
        qo <- queries_jq_deims[[get_deims_API_version()]]$location_info_multiPolygon
        jj <- get_id(location_id, qo$path)
        location <- dplyr::as_tibble(do_Q(qo$query, jj)) %>%
          dplyr::mutate(
            locationType.label = "not declared",
            locationType.uri = NA,
            .before = locationType
          ) %>%
          dplyr::select(-c(
            "locationType"
          ))
      }
    } else {
      locationType <- types$locationType.label
      if (geometryType == "Point") {
        qo <- queries_jq_deims[[get_deims_API_version()]]$location_info_point
        jj <- get_id(location_id, qo$path)
        location <- dplyr::as_tibble(do_Q(qo$query, jj))
      } else if (geometryType == "Polygon") {
        qo <- queries_jq_deims[[get_deims_API_version()]]$location_info_polygon
        jj <- get_id(location_id, qo$path)
        location <- dplyr::as_tibble(do_Q(qo$query, jj))
      } else if (geometryType == "MultiPolygon") {
        qo <- queries_jq_deims[[get_deims_API_version()]]$location_info_multiPolygon
        jj <- get_id(location_id, qo$path)
        location <- dplyr::as_tibble(do_Q(qo$query, jj))
      }
    }
    # harmonization of date and time
    location$created <- lubridate::as_datetime(location$created)
    location$changed <- lubridate::as_datetime(location$changed)
    location$relatedSite.changed <- lubridate::as_datetime(
      location$relatedSite.changed
    )
    # units for elevation.min and elevation.max fields
    location$elevation.min <- units::set_units(
      location$elevation.min, "m"
    )
    location$elevation.max <- units::set_units(
      location$elevation.max, "m"
    )
    # merge prefix end suffix
    location <- location %>%
      dplyr::mutate(
        relatedSite.uri = paste0(
          location$relatedSite.id.prefix,
          location$relatedSite.id.suffix
        ),
        .after = relatedSite.title
      ) %>%
      dplyr::select(-c(
        "relatedSite.id.prefix",
        "relatedSite.id.suffix"
      ))
    if (!is.null(location)) {
      if (is.na(location$coordinates[1])) {
        message("\n---- This location don't contains geo info. ----\n") # nocov
        geoLocation <- location
        map <- NULL
      } else {
        if (location$geometryType[1] == "Point") {
          location$boundaries <- sf::st_as_text(
            sf::st_point(
              location$coordinates[[1]]
            )
          )
        } else if (location$geometryType[1] == "Polygon") {
          location$boundaries <- sf::st_as_text(
            sf::st_polygon(
              location$coordinates
            )
          )
        } else if (location$geometryType[1] == "MultiPolygon") {
          location$boundaries <- sf::st_as_text(
            sf::st_multipolygon(
              list(location$coordinates),
              dim = "XY"
            )
          )
        }
        location <- location %>%
          dplyr::select(
            -c("geometryType",
               "coordinates")
          ) %>%
          dplyr::slice(1L)
        geoLocation <- sf::st_as_sf(
          location,
          wkt = "boundaries",
          crs = 4326
        )
        map <- leaflet::leaflet(geoLocation) %>%
          leaflet::addTiles()
        if (sf::st_geometry_type(geoLocation) == "POINT") {
          if (types$locationType.label == "not declared") {
            map <- map %>%
              leaflet::addCircleMarkers(
                data = geoLocation,
                radius = 8,
                color = "white",
                weight = 3,
                opacity = 1,
                fill = TRUE,
                fillColor = "red",
                fillOpacity = 0.6,
                popup = paste0(
                  "<b>Location title: </b>",
                  "<br>",
                  geoLocation$title,
                  "<br>",
                  "<b>Location type: </b>",
                  "<br>",
                  geoLocation$locationType.label,
                  "<br>",
                  "<b>Related ",
                  geoLocation$relatedSite.type,
                  ": </b><br>",
                  "<a href='",
                  geoLocation$relatedSite.uri,
                  "' target='_blank'>",
                  geoLocation$relatedSite.title,
                  "</a>"
                )
              )
          } else if (types$locationType.label == "Sampling Location") {
              map <- map %>%
                leaflet::addCircleMarkers(
                  data = geoLocation,
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
                    geoLocation$title,
                    "<br>",
                    "<b>Location type: </b>",
                    "<br>",
                    geoLocation$locationType.label,
                    "<br>",
                    "<b>Related ",
                    geoLocation$relatedSite.type,
                    ": </b><br>",
                    "<a href='",
                    geoLocation$relatedSite.uri,
                    "' target='_blank'>",
                    geoLocation$relatedSite.title,
                    "</a>"
                  )
                )
            } else if (types$locationType.label == "Equipment Location") {
              map <- map %>%
                leaflet::addCircleMarkers(
                  data = geoLocation,
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
                    geoLocation$title,
                    "<br>",
                    "<b>Location type: </b>",
                    "<br>",
                    geoLocation$locationType.label,
                    "<br>",
                    "<b>Related ",
                    geoLocation$relatedSite.type,
                    ": </b><br>",
                    "<a href='",
                    geoLocation$relatedSite.uri,
                    "' target='_blank'>",
                    geoLocation$relatedSite.title,
                    "</a>"
                  )
                )
            }
        } else if (sf::st_geometry_type(geoLocation) == "POLYGON") {
          if (types$locationType.label == "not declared") {
            map <- map %>%
              leaflet::addPolygons(
                data = geoLocation,
                color = "white",
                weight = 3,
                opacity = 1,
                fill = TRUE,
                fillColor = "red",
                fillOpacity = 0.6,
                popup = paste0(
                  "<b>Location title: </b>",
                  "<br>",
                  geoLocation$title,
                  "<br>",
                  "<b>Location type: </b>",
                  "<br>",
                  geoLocation$locationType.label,
                  "<br>",
                  "<b>Related ",
                  geoLocation$relatedSite.type,
                  ": </b><br>",
                  "<a href='",
                  geoLocation$relatedSite.uri,
                  "' target='_blank'>",
                  geoLocation$relatedSite.title,
                  "</a>"
                )
              )
          } else if (types$locationType.label == "Sampling Location") {
              map <- map %>%
                leaflet::addPolygons(
                  data = geoLocation,
                  color = "white",
                  weight = 3,
                  opacity = 1,
                  fill = TRUE,
                  fillColor = "#336600",
                  fillOpacity = 0.6,
                  popup = paste0(
                    "<b>Location title: </b>",
                    "<br>",
                    geoLocation$title,
                    "<br>",
                    "<b>Location type: </b>",
                    "<br>",
                    geoLocation$locationType.label,
                    "<br>",
                    "<b>Related ",
                    geoLocation$relatedSite.type,
                    ": </b><br>",
                    "<a href='",
                    geoLocation$relatedSite.uri,
                    "' target='_blank'>",
                    geoLocation$relatedSite.title,
                    "</a>"
                  )
                )
            } else if (types$locationType.label == "Equipment Location") {
              map <- map %>%
                leaflet::addPolygons(
                  data = geoLocation,
                  color = "white",
                  weight = 3,
                  opacity = 1,
                  fill = TRUE,
                  fillColor = "#1A1AFF",
                  fillOpacity = 0.6,
                  popup = paste0(
                    "<b>Location title: </b>",
                    "<br>",
                    geoLocation$title,
                    "<br>",
                    "<b>Location type: </b>",
                    "<br>",
                    geoLocation$locationType.label,
                    "<br>",
                    "<b>Related ",
                    geoLocation$relatedSite.type,
                    ": </b><br>",
                    "<a href='",
                    geoLocation$relatedSite.uri,
                    "' target='_blank'>",
                    geoLocation$relatedSite.title,
                    "</a>"
                  )
                )
            }
        } else if (sf::st_geometry_type(geoLocation) == "MULTIPOLYGON") {
          if (types$locationType.label == "not declared") {
            map <- map %>%
              leaflet::addPolygons(
                data = geoLocation,
                color = "white",
                weight = 3,
                opacity = 1,
                fill = TRUE,
                fillColor = "red",
                fillOpacity = 0.6,
                popup = paste0(
                  "<b>Location title: </b>",
                  "<br>",
                  geoLocation$title,
                  "<br>",
                  "<b>Location type: </b>",
                  "<br>",
                  geoLocation$locationType.label,
                  "<br>",
                  "<b>Related ",
                  geoLocation$relatedSite.type,
                  ": </b><br>",
                  "<a href='",
                  geoLocation$relatedSite.uri,
                  "' target='_blank'>",
                  geoLocation$relatedSite.title,
                  "</a>"
                )
              )
          } else if (types$locationType.label == "Sampling Location") {
              map <- map %>%
                leaflet::addPolygons(
                  data = geoLocation,
                  color = "white",
                  weight = 3,
                  opacity = 1,
                  fill = TRUE,
                  fillColor = "#336600",
                  fillOpacity = 0.6,
                  popup = paste0(
                    "<b>Location title: </b>",
                    "<br>",
                    geoLocation$title,
                    "<br>",
                    "<b>Location type: </b>",
                    "<br>",
                    geoLocation$locationType.label,
                    "<br>",
                    "<b>Related ",
                    geoLocation$relatedSite.type,
                    ": </b><br>",
                    "<a href='",
                    geoLocation$relatedSite.uri,
                    "' target='_blank'>",
                    geoLocation$relatedSite.title,
                    "</a>"
                  )
                )
            } else if (types$locationType.label == "Equipment Location") {
              map <- map %>%
                leaflet::addPolygons(
                  data = geoLocation,
                  color = "white",
                  weight = 3,
                  opacity = 1,
                  fill = TRUE,
                  fillColor = "#1A1AFF",
                  fillOpacity = 0.6,
                  popup = paste0(
                    "<b>Location title: </b>",
                    "<br>",
                    geoLocation$title,
                    "<br>",
                    "<b>Location type: </b>",
                    "<br>",
                    geoLocation$locationType.label,
                    "<br>",
                    "<b>Related ",
                    geoLocation$relatedSite.type,
                    ": </b><br>",
                    "<a href='",
                    geoLocation$relatedSite.uri,
                    "' target='_blank'>",
                    geoLocation$relatedSite.title,
                    "</a>"
                  )
                )
            }
        }
      }
    } else {
      geoLocation <- NULL
      map <- NULL
    }
  } else {
    stop("\n----\nPage Not Found. The requested page could not be found. Please
check again the location.iD\n----\n")
  }
  if (show_map == TRUE) {
    print(map)
    geoLocation
  } else {
    geoLocation
  }
}
