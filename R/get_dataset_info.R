#' Obtain the information about of an eLTER dataset.
#' @description This function obtains the information about of an eLTER
#' dataset (e.g.
#' \url{https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845})
#' provided in \href{https://deims.org/}{DEIMS-SDR catalogue}.
#' @param datasetid A `character`. It is the DEIMS.iD of dataset make from
#' DEIMS-SDR website. DEIMS.iD information 
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' The DEIMS.iD of dataset is the URL for the dataset page.
#' @return The output of the function is a `tibble` with main features
#' of the site and the related resources collected by site.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr RETRY content
#' @importFrom dplyr as_tibble
#' @importFrom utils capture.output
#' @importFrom sf st_as_sf st_is_valid
#' @importFrom leaflet leaflet addTiles addPolygons
#' @importFrom jqr jq
#' @importFrom jsonlite stream_in
#' @export
#' @examples
#' tDataset <- get_dataset_info(
#'   datasetid =
#'   "https://deims.org/dataset/38d604ef-decb-4d67-8ac3-cc843d10d3ef"
#' )
#' tDataset
#'
### function get_dataset_info
get_dataset_info <- function(datasetid) {
  q <- '{
       title: .title,
       abstract: .attributes.general.abstract,
       keywords: .attributes.general.keywords,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       type: .type,
       dateRange: .attributes.general.dateRange,
       relatedSite: .attributes.general.relatedSite,
       contacts: .attributes.contact,
       observationParameters: .attributes.observations.parameters,
       observationSpecies: .attributes.observations.speciesGroups,
       dataPolicy: .attributes.onlineDistribution.dataPolicyUrl,
       doi: .attributes.onlineDistribution.doi,
       onlineLocation: .attributes.onlineDistribution.onlineLocation,
       legal: .attributes.legal,
       method: .attributes.method,
       boundaries: .attributes.geographic[].boundaries,
       boundariesDescription: .attributes.geographic[].abstract
      }'
  jj <- get_id(datasetid, "datasets")
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        dataset <- dplyr::as_tibble(do_Q(q, jj))
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
          print(map)
          geoDataset
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
  print(map)
  geoDataset
}
