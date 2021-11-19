#' @title eLTER get_ilter_parameters function
#' @description This function obtains information about the
#' parameters collected in ILTER sites, through the DEIMS-SDR sites API.
#' @param sitesNum A `integer`. It is the number of the sites that are
#' read to get the information. Use this parameters moreover for provide
#' example of this function. Default 0.
#' @return The output of the function is a `tibble` containing the list
#' of parameters and their URI (Uniform Resource Identifier) collected
#' in all ILTER sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows distinct
#' @export
#' @examples
#' \dontrun{
#' listParams <- get_ilter_parameters(sitesNum = 20)
#' listParams[1:10, ] %>%
#'   dplyr::rows_insert(
#'   dplyr::tibble(
#'     parameterLabel = "...", parameterUri = "..."
#'   )
#' )
#' }
#'
### function get_ilter_parameters
get_ilter_parameters <- function(sitesNum = 0) {
  if (sitesNum == 0) {
    lterILTERSites <- as.list(jsonlite::fromJSON("https://deims.org/api/sites"))
    allSiteParameters <- lapply(
      as.list(
        paste0(
          lterILTERSites$id$prefix,
          lterILTERSites$id$suffix
        )
      ),
      ReLTER::get_site_info,
      category = "Parameters"
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
      ReLTER::get_site_info,
      category = "Parameters"
    )
    uniteSiteParameters <- dplyr::bind_rows(allSiteParameters)
    parametersILTERList <- uniteSiteParameters$parameter
    parametersILTERDF <- dplyr::bind_rows(parametersILTERList)
    uniqueSitesParameters <- dplyr::as_tibble(
      dplyr::distinct(parametersILTERDF)
    )
    uniqueSitesParameters
  } else {
    message("\n----\nThe `sitesNum` value must be a double (e.g. 10, 24, etc.).
Please check again the value of `sitesNum`.\n----\n")
    uniqueSitesParameters <- NULL
  }
}
