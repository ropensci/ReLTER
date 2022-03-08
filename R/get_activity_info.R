#' @title Return a tibble object containing information, as a stored in
#' \href{https://deims.org/}{DEIMS-SDR catalogue}, about a single eLTER
#' activity (e.g.
#' \url{https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845}).
#' @description This function obtains the info of activity (actually
#'  title) provided in DEIMS-SDR.
#' @param activityid A character. The DEIMS activity ID from
#' DEIMS-SDR website. DEIMS.iD information 
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of
#' the activities in a site, and a `leaflet` map plot.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr RETRY content
#' @importFrom dplyr as_tibble
#' @importFrom utils capture.output
#' @importFrom sf st_as_sf st_is_valid
#' @importFrom leaflet leaflet addTiles addPolygons
#' @export
#' @examples
#' activities <- get_activity_info(
#'   activityid =
#'   "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845"
#' )
#' activities
#'
### function get_activity_info
get_activity_info <- function(activityid) {
  q <- "{
        title: .title,
        boundaries: .attributes.geographic.boundaries
        }"
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
            leaflet::addTiles() %>%
            leaflet::addPolygons()
          print(map)
          geoActivity
        } else {
          map <- leaflet::leaflet() %>%
            leaflet::addTiles()
          message("\n----\nThe maps cannot be created because the polygon of
activity, provided in DEIMS-SDR, has an invalid geometry.
Please check the content and refers this error to DEIMS-SDR
contact person of the activity, citing the Activity.iD.\n----\n")
          print(map)
          geoActivity
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
  print(map)
  geoActivity
}
