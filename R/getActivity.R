#' @title eLTER getActivity function
#' @description This function obtains the info of activity (actually
#'  title) provided in DEIMS-SDR.
#' @param activityid A character. The DEIMS activity ID from
#' DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features of
#' the activities in a site, and a `leaflet` map plot.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr GET content
#' @importFrom dplyr as_tibble
#' @importFrom utils capture.output
#' @importFrom sf st_as_sf
#' @importFrom leaflet leaflet addTiles addPolygons
#' @importFrom rgeos gIsValid
#' @export
#' @examples
#' activities <- getActivity(activityid = "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845")
#' map <- leaflet::leaflet(activities) %>% 
#'  leaflet::addTiles() %>% 
#'  leaflet::addPolygons()
#' print(map)
#' activities
#'
### function getActivity
getActivity <- function(activityid) {
  require(dplyr)
  q <- "{
        title: .title,
        boundaries: .attributes.geographic.boundaries
        }"
  url <- paste0(
    "https://deims.org/",
    "api/activities/",
    sub("^.+/", "", activityid)
  )
  export <- httr::GET(url = url)
  jj <- suppressMessages(httr::content(export, "text"))
  status <- jj %>% 
    jqr::jq(as.character('{status: .errors.status}')) %>% 
    textConnection() %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE) %>%
    dtplyr::lazy_dt() %>% 
    dplyr::as_tibble()
  if (is.na(status)) {
    invisible(
      utils::capture.output(
        activity <- dplyr::as_tibble(ReLTER:::do_Q(q, jj))
      )
    )
    if (!is.null(activity)) {
      if (is.na(activity$boundaries)) {
        message("\n---- This activity don't contains geo info. ----\n")
        geoActivity <- activity
        map <- NULL
      } else {
        geoActivity <- sf::st_as_sf(
          activity,
          wkt = "boundaries",
          crs = 4326
        )
        geoActivity_SP <- sf::as_Spatial(
          geoActivity$boundaries
        )
        geoActivity_valid <- rgeos::gIsValid(
          geoActivity_SP,
          byid = FALSE,
          reason = TRUE
        )
        if (geoActivity_valid == "Valid Geometry") {
          map <- leaflet::leaflet(geoActivity) %>%
            leaflet::addTiles() %>% 
            leaflet::addPolygons()
          print(map)
          geoActivity
        } else {
          map <- leaflet::leaflet() %>%
            leaflet::addTiles()
          message("\n----\n The maps cannot be created because the polygon of activity, provided in DEIMS-SDR, has an invalid geometry.\n Please check the content and refers this error to DEIMS-SDR contact person of the activity, citing the Activity.iD.\n----\n")
          print(map)
          geoActivity
        }
      }
    } else {
      geoActivity <- NULL
      map <- NULL
    }
  } else {
    message("\n---- The requested page could not be found. Please check again the Activity.iD ----\n")
    geoActivity <- NULL
    map <- NULL
  }
  print(map)
  geoActivity
}
