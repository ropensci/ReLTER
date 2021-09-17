#' @title eLTER getILTERParameters function
#' @description This function allows to obtain the information about the
#' parameters collected in all sites of ILTER, througth the DEIMS-SDR
#' sites API.
#' @param null function witout parameter
#' @return The output of the function is a `tibble` containing the list
#' of parameters and their URI (Uniform Resource Identifier) collected
#' in all ILTER sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import jsonlite dplyr
#' @export
#' @examples
#' require('dplyr')
#' listParams <- getILTERParameters()
#' listParams[1:10, ] %>%
#'   dplyr::rows_insert(
#'   dplyr::tibble(
#'     parameterLabel = "...", parameterUri = "..."
#'   )
#' )
#'
### function getILTERParameters
getILTERParameters <- function() {
  lterILTERSites <- as.list(jsonlite::fromJSON("https://deims.org/api/sites"))
  allSiteParameters <- lapply(
    as.list(
      paste0(
        lterILTERSites$id$prefix,
        lterILTERSites$id$suffix
      )
    ),
    ReLTER::getSiteParameters
  )
  uniteSiteParameters <- dplyr::bind_rows(allSiteParameters)
  parametersILTERList <- uniteSiteParameters$parameter
  parametersILTERDF <- dplyr::bind_rows(parametersILTERList)
  uniqueSitesParameters <- tibble::as_tibble(dplyr::distinct(parametersILTERDF))
  uniqueSitesParameters
}
