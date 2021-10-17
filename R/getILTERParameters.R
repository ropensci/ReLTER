#' @title eLTER getILTERParameters function
#' @description This function allows to obtain the information about the
#' parameters collected in all sites of ILTER, througth the DEIMS-SDR
#' sites API.
#' @param sitesNum A `integer`. It is the number of the sites that are 
#' read to get the information. Use this parameters oreover for provide
#' example of this function.
#' @return The output of the function is a `tibble` containing the list
#' of parameters and their URI (Uniform Resource Identifier) collected
#' in all ILTER sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows distinct
#' @export
#' @examples
#' \donttest
#' require('dplyr')
#' listParams <- getILTERParameters(sitesNum = 20)
#' listParams[1:10, ] %>%
#'   dplyr::rows_insert(
#'   dplyr::tibble(
#'     parameterLabel = "...", parameterUri = "..."
#'   )
#' )
#' \donttest
#'
### function getILTERParameters
getILTERParameters <- function(sitesNum = NULL) {
  if (is.na(sitesNum)) {
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
    uniqueSitesParameters <- dplyr::as_tibble(
      dplyr::distinct(parametersILTERDF)
    )
    uniqueSitesParameters
  } else if (typeof(sitesNum) == "double") {
    lterILTERSites <- as.list(jsonlite::fromJSON("https://deims.org/api/sites"))
    allSiteParameters <- lapply(
      as.list(
        paste0(
          lterILTERSites$id$prefix[1:sitesNum],
          lterILTERSites$id$suffix[1:sitesNum]
        )
      ),
      ReLTER::getSiteParameters
    )
    uniteSiteParameters <- dplyr::bind_rows(allSiteParameters)
    parametersILTERList <- uniteSiteParameters$parameter
    parametersILTERDF <- dplyr::bind_rows(parametersILTERList)
    uniqueSitesParameters <- dplyr::as_tibble(
      dplyr::distinct(parametersILTERDF)
    )
    uniqueSitesParameters
  } else {
    message("\n---- The `sitesNum` value must be a double (e.g. 10, 24, etc.). Please check again the value of `sitesNum`. ----\n")
    uniqueSitesParameters <- NULL
  }
}

# TODO: occorre mettere un controllo di errore tipo "status" nelle funzioni getSite...?
# In questo caso l'URL su cui viene fatta la chiamata non è parametrizzata e quindi
#   non credo che sia da inserire alcun controllo. Vero?
