#' Obtain information about the parameters collected of all sites ILTER.
#' @description Return a `tibble` object containing Parameters collected by all
#' of the \href{https://www.ilter.network/network/global-coverage}{ILTER sites
#' (more than 1200 around the world)}, available from
#' \href{https://deims.org}{DEIMS-SDR}.
#'
#' This function gathers in a unique tibble all the Parameters
#' from all ILTER sites. Note that the execution time for this function
#' is very high.
#'
#' If the objective is obtain information about Parameters
#' on a few sites, it is better to use other more specific functions (e.g.
#' \href{get_network_parameters.html}{`get_network_parameters()`} or
#' \href{get_site_info.html}{`get_site_info()`}) or using
#' other methods
#' (\href{../../articles/sites_information.html}{How to about sites
#' informations}).
#' @param sitesNum A `integer`. The number of the sites that are
#' read to get the information. Use this parameter only to sample the output
#' of this function. If the value of sitesNum is #' 0 (default)
#' all the ILTER sites will be parsed and the waiting time will be long.
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
    message("\n----\nThe `sitesNum` value must be a numeric (e.g. 10, 24, etc.).
Please check again the value of `sitesNum`.\n----\n")
    uniqueSitesParameters <- NULL
  }
}
