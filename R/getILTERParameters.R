#' @title eLTER_getILTERParameters
#' @description This function ...
#' @param ...
#' @return The output of the function is ...
#' @author Alessandro Oggioni, phD (2020) <oggioniale@gmail.com>
#' @import jsonlite
#' @export
#' @examples
#' getILTERParameters()
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
  uniqueSitesParameters <- dplyr::distinct(parametersILTERDF)
  uniqueSiteParameters
}
