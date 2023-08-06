#' Obtain the information about of an eLTER sensor.
#' @description `r lifecycle::badge("experimental")`
#' This function obtains the information about of an eLTER
#' sensor (e.g.
#' \url{https://deims.org/sensors/3845475c-4aec-4dd7-83b4-0ab6ba95db35})
#' provided in \href{https://deims.org/}{DEIMS-SDR catalogue}.
#' @param sensorid A `character`. It is the DEIMS ID of sensor make from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' The DEIMS.iD of sensor is the URL for the sensor page.
#' @param show_map A `boolean`. If TRUE a Leaflet map with occurrences
#' is shown. Default FALSE.
#' @return The output of the function is a `tibble` with main features of
#' the activities in a site, and a `leaflet` map plot.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom dplyr as_tibble
#' @importFrom utils capture.output
#' @importFrom sf st_as_sf st_is_valid
#' @importFrom leaflet leaflet addTiles addMarkers
#' @importFrom lubridate as_datetime
#' @export
#' @examples
#' # only table of sensor information
#' sensor_B3 <- get_sensor_info(
#'   sensorid =
#'     "https://deims.org/sensors/3845475c-4aec-4dd7-83b4-0ab6ba95db35",
#'   show_map = TRUE
#' )
#' sensor_B3
#' 
#' # print the map of the sensor
#' Licor <- get_sensor_info(
#'   sensorid =
#'     "https://deims.org/sensors/4a7ad644-f2e7-4224-965b-ec5ef5365655",
#'   show_map = FALSE
#' )
#' Licor
#' 
#' # Moldaenke FluoroProbe sensor
#' sensor_FP <- get_sensor_info(
#'   sensorid = "https://deims.org/sensors/82635223-a4f4-498c-b283-9c95999d9d2f",
#'   show_map = FALSE
#' )
#' sensor_FP
#'
#' @section The function output:
#' \figure{get_sensor_info_fig.png}{Map of position of the "Climate Station B3" sensor}
#'
### function get_sensor_info
get_sensor_info <- function(sensorid, show_map = FALSE) {
  qo <- queries_jq[[get_deims_API_version()]]$sensor_info
  jj <- get_id(sensorid, qo$path)
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        sensor <- dplyr::as_tibble(do_Q(qo$query, jj))
      )
    )
    # harmonization of date and time
    sensor$created <- lubridate::as_datetime(sensor$created)
    sensor$changed <- lubridate::as_datetime(sensor$changed)
    if (!is.null(sensor)) {
      if (is.na(sensor$geography)) {
        message("\n---- This sensor don't contains geo info. ----\n") # nocov
        geoSensor <- sensor
        map <- NULL
      } else {
        geoSensor <- sf::st_as_sf(
          sensor,
          wkt = "geography",
          crs = 4326
        )
        geoSensor_valid <- sf::st_is_valid(geoSensor)
        if (any(geoSensor_valid)) {
          map <- leaflet::leaflet(geoSensor) %>%
            leaflet::addTiles() %>%
            leaflet::addMarkers(
              popup = paste0(
                "<b>Sensor title: </b>"
                , geoSensor$title
                , "<br>"
                , "<a href='"
                , geoSensor$uri
                , "' target='_blank'>"
                , "Click Here to View sensor landing page</a>"
              )
            )
        } else {
          map <- leaflet::leaflet() %>%
            leaflet::addTiles()
          message("\n----\nThe maps cannot be created because the geo info of
sensor, provided in DEIMS-SDR, has an invalid geometry.
Please check the content and refers this error to DEIMS-SDR
contact person in charge of the sensor, citing the Sensor.iD.\n----\n")
        }
      }
    } else {
      geoSensor <- NULL
      map <- NULL
    }
  } else {
    stop("\n----\nPage Not Found. The requested page could not be found. Please
check again the Sensor.iD\n----\n")
  }
  if (show_map == TRUE) {
    print(map)
    geoSensor
  } else {
    geoSensor
  }
}
