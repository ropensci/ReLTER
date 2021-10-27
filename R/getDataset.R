#' @title eLTER getDataset function
#' @description This function allows to obtain the info of dataset provided in
#' DEIMS-SDR.
#' @param datasetid A character. It is the DEIMS iD of dataset make from
#' DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features
#' of the site and the related resources collected by site.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr GET content
#' @importFrom dplyr as_tibble
#' @importFrom utils capture.output
#' @importFrom sf st_as_sf
#' @importFrom leaflet leaflet addTiles addPolygons
#' @importFrom magrittr %>%
#' @export
#' @examples
#' tDataset <- getDataset(datasetid = "https://deims.org/dataset/38d604ef-decb-4d67-8ac3-cc843d10d3ef")
#' map <- leaflet::leaflet(tDataset) %>% 
#'  leaflet::addTiles() %>% 
#'  leaflet::addPolygons()
#' print(map)
#' tDataset
#'
### function getDataset
getDataset <- function(datasetid) {
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
  url <- paste0(
    "https://deims.org/",
    "api/datasets/",
    sub("^.+/", "", datasetid)
  )
  export <- httr::GET(url = url)
  jj <- suppressMessages(httr::content(export, as = "text", encoding = "UTF-8"))
  status <- jj %>% 
    jqr::jq(as.character('{status: .errors.status}')) %>% 
    textConnection() %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE) %>%
    dtplyr::lazy_dt() %>% 
    dplyr::as_tibble()
  if (is.na(status)) {
    invisible(
      utils::capture.output(
        dataset <- dplyr::as_tibble(ReLTER:::do_Q(q, jj))
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
        geoDataset_SP <- sf::as_Spatial(
          geoDataset$boundaries
        )
        geoDataset_valid <- rgeos::gIsValid(
          geoDataset_SP,
          byid = FALSE,
          reason = TRUE
        )
        if (geoDataset_valid == "Valid Geometry") {
          map <- leaflet::leaflet(geoDataset) %>%
            leaflet::addTiles() %>%
            leaflet::addPolygons()
          print(map)
          geoDataset
        } else {
          map <- leaflet::leaflet() %>%
            leaflet::addTiles()
          message("\n----\n The maps cannot be created because the polygon of dataset, provided in DEIMS-SDR, has an invalid geometry.\n Please check the content and refers this error to DEIMS-SDR contact person of dataset, citing the Dataset.iD.\n----\n")
          print(map)
          geoDataset
        }
      }
    } else {
      geoDataset <- NULL
      map <- NULL
    }
  } else {
    message("\n---- The requested page could not be found. Please check again the Dataset.iD ----\n")
    geoDataset <- NULL
    map <- NULL
  }
  print(map)
  geoDataset
}
