#' Obtain the information about of an eLTER activity.
#' @description This function obtains the information about of an eLTER
#' activity (e.g.
#' \url{https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845})
#' provided in \href{https://deims.org/}{DEIMS-SDR catalogue}.
#' @param activityid A `character`. It is the DEIMS ID of activity make from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' The DEIMS.iD of activity is the URL for the activity page.
#' @param show_map A `boolean`. If TRUE a Leaflet map with occurrences
#' is shown. Default FALSE.
#' @return The output of the function is a `tibble` with main features of
#' the activities in a site, and a `leaflet` map plot.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr RETRY content
#' @importFrom dplyr as_tibble
#' @importFrom utils capture.output
#' @importFrom sf st_as_sf st_is_valid
#' @importFrom leaflet leaflet addTiles addPolygons
#' @importFrom Rdpack reprompt
#' @references
#'   \insertRef{httrR}{ReLTER}
#'
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
#'   "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845",
#'   show_map = FALSE
#' )
#' activities
#'
#' @section The function output:
#' \figure{get_activity_info_fig.png}{Map of "Study of non-indigenous (alien)
#' species in the Mar Piccolo of Taranto" activity}
#'
### function get_activity_info
get_activity_info <- function(activityid, show_map = FALSE) {
  q <- '{
        title: .title,
        abstract: .attributes.general.abstract,
        keywords: .attributes.general.keywords,
        uri: "\\(.id.prefix)\\(.id.suffix)",
        type: .type,
        created: .created,
        changed: .changed,
        relatedSite: .attributes.general.relatedSite,
        contacts: .attributes.contact,
        boundaries: .attributes.geographic.boundaries,
        observationParameters: .attributes.observations.parameters,
        relatedResources: .attributes.relatedResources
        }'
  jj <- get_id(activityid, "activities")
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        activity <- dplyr::as_tibble(do_Q(q, jj))
      )
    )
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
      geoActivity <- NULL
      map <- NULL
    }
  } else {
    stop("\n----\nPage Not Found. The requested page could not be found. Please
check again the Activity.iD\n----\n")
  }
  if (show_map == TRUE) {
    print(map)
    geoActivity
  } else {
    geoActivity
  }
}
