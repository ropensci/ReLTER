#' Obtain the information about of an eLTER activity.
#' @description `r lifecycle::badge("stable")`
#' This function obtains the information about of an eLTER
#' activity (e.g.
#' \url{https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845})
#' provided in \href{https://deims.org/}{DEIMS-SDR catalogue}.
#' @param activityid A `character`. It is the DEIMS ID of activity make from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' The DEIMS.iD of activity is the URL for the activity page.
#' @param show_map A `boolean`. If TRUE a Leaflet map with occurrences
#' is shown. Default FALSE.
#' @return The output of the function is a `list` with two elements:
#' \itemize{
#' \item \code{map} A Leaflet map with the activity location, if requested with
#' `show_map`.
#' \item \code{data} A `data.frame` with the information about the activity.
#' }
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom dplyr as_tibble
#' @importFrom utils capture.output
#' @importFrom sf st_as_sf st_is_valid
#' @importFrom leaflet leaflet addTiles addPolygons
#' @importFrom Rdpack reprompt
#' @importFrom lubridate as_datetime
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
#' activities <- get_activity_info(
#'   activityid =
#'     "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845",
#'   show_map = TRUE
#' )
#' activities
#'
#' @section The function output:
#' \figure{get_activity_info_fig.png}{Map of "Study of non-indigenous (alien)
#' species in the Mar Piccolo of Taranto" activity}
#'
### function get_activity_info
get_activity_info <- function(activityid, show_map = FALSE) {
  qo <- queries_jq_deims[[get_deims_API_version()]]$activity_info
  jj <- get_id(activityid, qo$path)
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        activity <- dplyr::as_tibble(do_Q(qo$query, jj))
      )
    )
    # harmonization of date and time
    if (!is.na(activity$created)) {
      activity$created <- lubridate::as_datetime(activity$created)
    }
    if (!is.na(activity$changed)) {
      activity$changed <- lubridate::as_datetime(activity$changed)
    }
    if (!is.na(activity$relatedSite)) {
      activity$relatedSite[[1]]$changed <- lubridate::as_datetime(activity$relatedSite[[1]]$changed)
    }
    if (!is.na(activity$relatedResources)) {
      activity$relatedResources[[1]]$changed <- lubridate::as_datetime(activity$relatedResources[[1]]$changed)
    }
    if (!is.na(activity$dateRange.from)) {
      activity$dateRange.from <- lubridate::as_date(activity$dateRange.from)
    }
    if (!is.na(activity$dateRange.to)) {
      activity$dateRange.to <- lubridate::as_date(activity$dateRange.to)
    }
    if (!is.null(activity)) {
      if (is.na(activity$boundaries)) {
        message("\n---- This activity don't contains geo info. ----\n") # nocov
        geoActivity <- activity
        map <- NULL
      } else {
        geoActivity <- sf::st_as_sf(
          activity,
          wkt = "boundaries",
          crs = 4326
        )
        geoActivity_valid <- sf::st_is_valid(geoActivity)
        if (any(geoActivity_valid)) {
          map <- leaflet::leaflet(geoActivity) %>%
            leaflet::addTiles()
            if (sf::st_geometry_type(geoActivity) == "POINT") {
              map <- map %>%
                leaflet::addMarkers()
            } else if (sf::st_geometry_type(geoActivity) == "POLYGON") {
              map <- map %>%
                leaflet::addPolygons()
            }
        } else {
          map <- leaflet::leaflet() %>%
            leaflet::addTiles()
          message("\n----\nThe maps cannot be created because the polygon of
activity, provided in DEIMS-SDR, has an invalid geometry.
Please check the content and refers this error to DEIMS-SDR
contact person of the activity, citing the Activity.iD.\n----\n")
        }
      }
    } else {
      return(list(
        map = NULL,
        data = NULL
      ))
    }
  } else {
    stop("\n----\nPage Not Found. The requested page could not be found. Please
check again the Activity.iD\n----\n")
  }
  if (show_map == TRUE) {
    return(list(
      map = map,
      data = geoActivity
    ))
  } else {
    return(list(
      map = NULL,
      data = geoActivity
    ))
  }
}
