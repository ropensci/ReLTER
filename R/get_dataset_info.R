#' Obtain the information about of an eLTER dataset.
#' @description `r lifecycle::badge("stable")`
#' This function obtains the information about of an eLTER
#' dataset (e.g.
#' \url{https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845})
#' provided in \href{https://deims.org/}{DEIMS-SDR catalogue}.
#' @param datasetid A `character`. It is the DEIMS ID of dataset make from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' The DEIMS ID of dataset is the URL for the dataset page.
#' @param show_map A `boolean`. If TRUE a Leaflet map with occurrences
#' is shown. Default FALSE.
#' @return The output of the function is a `tibble` with main features
#' of the site and the related resources collected by site.
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
#' tDataset <- get_dataset_info(
#'   datasetid =
#'   "https://deims.org/dataset/38d604ef-decb-4d67-8ac3-cc843d10d3ef",
#'   show_map = TRUE
#' )
#' tDataset
#'
#' @section The function output:
#' \figure{get_dataset_info_fig.png}{Map of "LTER Northern Adriatic Sea (Italy)
#' marine data from 1965 to 2015" dataset}
#'
### function get_dataset_info
get_dataset_info <- function(datasetid, show_map = FALSE) {
  qo <- queries_jq[[get_deims_API_version()]]$dataset_info
  jj <- get_id(datasetid, qo$path)
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        dataset <- dplyr::as_tibble(do_Q(qo$query, jj))
      )
    )
    # fix the observationParameters columns name
    if (!is.na(dataset$observationParameters)) {
      colnames(dataset$observationParameters[[1]]) <- c(
        "parametersLabel",
        "parametersUri"
      )
    } else {
      parametersLabel <- NA
      parametersUri <- NA
      dataset$observationParameters <- list(
        data.frame(
          parametersLabel,
          parametersUri
        )
      )
    }
    # fix the observationSpecies columns name
    if (!is.na(dataset$observationSpecies)) {
      colnames(dataset$observationSpecies[[1]]) <- c(
        "speciesLabel",
        "speciesUri"
      )
    } else {
      speciesLabel <- NA
      speciesUri <- NA
      dataset$observationSpecies <- list(
        data.frame(
          speciesLabel,
          speciesUri
        )
      )
    }
    # transform tibble into sf
    if (!is.null(dataset)) {
      if (is.na(dataset$boundaries)) {
        message("\n---- This dataset don't contains geo info. ----\n")
        geoDataset <- dataset
        map <- NULL
      } else {
        geoDataset <- sf::st_as_sf(dataset, wkt = "boundaries", crs = 4326)
        geoDataset_valid <- sf::st_is_valid(geoDataset)
        if (any(geoDataset_valid)) {
          map <- leaflet::leaflet(geoDataset) %>%
            leaflet::addTiles()
          if (sf::st_geometry_type(geoDataset) == "POINT") {
            map <- map %>%
              leaflet::addMarkers()
          } else if (sf::st_geometry_type(geoDataset) == "POLYGON") {
            map <- map %>%
              leaflet::addPolygons()
          }
        } else {
          map <- leaflet::leaflet() %>%
            leaflet::addTiles()
          message("\n----\nThe maps cannot be created because the polygon of
dataset, provided in DEIMS-SDR, has an invalid geometry.
Please check the content and refer this error to DEIMS-SDR support for this
dataset, citing the Dataset.iD.\n----\n")
          print(map)
          geoDataset
        }
      }
    } else {
      geoDataset <- NULL
      map <- NULL
    }
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the Dataset.iD\n----\n")
    geoDataset <- NULL
    map <- NULL
  }
  if (show_map == TRUE) {
    print(map)
    geoDataset
  } else {
    geoDataset
  }
}
