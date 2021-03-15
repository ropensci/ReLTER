#' @title eLTER_getDataset
#' @description This function allows to obtain the info of dataset provided in DEIMS-SDR.
#' @param datasetid A character. It is the DEIMS iD of dataset make from DEIMS-SDR website. More information about DEIMS iD in this \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features of the site and the related resources collected by site.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import tibble httr
#' @export
#' @examples
#' getDataset(datasetid = 'https://deims.org/datasets/75a7f938-7c77-11e3-8832-005056ab003f')
#'
### function getDataset
getDataset <- function(datasetid) {
  q = '{title: .title,
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
       method: .attributes.method
      }'
  # TODO add this fields:
  # boundaries: .attributes.geographic.boundaries,
  # nameOfBoundaries: .attributes.geographic.abstract
  url <- paste0("https://deims.org/", "api/", substring(datasetid, 19))
  export <- httr::GET(url = url)
  jj <- httr::content(export, "text")
  dataset <- tibble::as_tibble(do_Q(q, jj))
  if (!is.na(dataset$observationParameters)) {
    colnames(dataset$observationParameters[[1]]) <- c("parametersLabel", "parametersUri")
  } else {
    parametersLabel <- NA
    parametersUri <- NA
    dataset$observationParameters <- list(data.frame(parametersLabel, parametersUri))
  }
  if (!is.na(dataset$observationSpecies)) {
    colnames(dataset$observationSpecies[[1]]) <- c("speciesLabel", "speciesUri")
  } else {
    speciesLabel <- NA
    speciesUri <- NA
    dataset$observationSpecies <- list(data.frame(speciesLabel, speciesUri))
  }
  dataset
}

