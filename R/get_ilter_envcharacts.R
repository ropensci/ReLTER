#' Obtain a list of all Environmental Characteristics of ILTER sites.
#' @description This function obtains all Environmental Characteristics:
#' title, URI, geo-coordinates, country name, and elevation
#' of all
#' \href{https://www.ilter.network/network/global-coverage}{ILTER sites
#' (more than 1200 around the world)},
#' through the DEIMS-SDR API.
#' 
#' This function aims to gather in a unique tibble all the Environmental
#' Characteristics the whole of ILTER sites. If you want to obtain this
#' results, it should be noted that the execution time for this function
#' is very high.
#' 
#' If the objective is obtain information about Environmental Characteristics
#' on a few sites, it is better to use other more specific functions (e.g.
#' \href{get_network_envcharacts.html}{`get_network_envcharacts()`} or
#' \href{get_site_info.html}{`get_site_info()`}) or using
#' other methods
#' (\href{../../articles/sites_information.html}{How to about sites informations}).
#' @param sitesNum A `integer`. It is the number of the sites that are
#' read to get the information. Use this parameter only for get
#' example of the output of this function. If the value of sitesNum is
#' 0 (default) all the ILTER sites will be parsed and the waiting time will
#' be long.
#' @return The output of the function is a `tibble` containing all the
#' Environmental Characteristics of ILTER's sites.
#' @author Alessandro Oggioni, phD (2020) <oggioni.a@@irea.cnr.it>
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \dontrun{
#' listEnvCharacts <- get_ilter_envcharacts(sitesNum = 10)
#' listEnvCharacts[1:20, ]
#' }
#'
### function get_ilter_envcharacts
get_ilter_envcharacts <- function(sitesNum = 0) {
  if (sitesNum == 0) {
    lterILTERSites <- as.list(
      jsonlite::fromJSON("https://deims.org/api/sites")
    )
    allSiteEnvCharacts <- lapply(
      as.list(
        paste0(
          lterILTERSites$id$prefix,
          lterILTERSites$id$suffix
        )
      ),
      ReLTER::get_site_info,
      category = "EnvCharacts"
    )
    allSiteEnvCharacts_matrix <- do.call(rbind, allSiteEnvCharacts)
    allSiteEnvCharacts <- dplyr::as_tibble(
      as.data.frame(
        allSiteEnvCharacts_matrix
      )
    )
    allSiteEnvCharacts
  } else if (typeof(sitesNum) == "double") {
    lterILTERSites <- as.list(
      jsonlite::fromJSON("https://deims.org/api/sites")
    )
    allSiteEnvCharacts <- lapply(
      as.list(
        paste0(
          lterILTERSites$id$prefix[1:sitesNum],
          lterILTERSites$id$suffix[1:sitesNum]
        )
      ),
      ReLTER::get_site_info,
      category = "EnvCharacts"
    )
    allSiteEnvCharacts_matrix <- do.call(rbind, allSiteEnvCharacts)
    allSiteEnvCharacts <- dplyr::as_tibble(
      as.data.frame(
        allSiteEnvCharacts_matrix
      )
    )
    allSiteEnvCharacts
  } else {
    message("\n----\nThe `sitesNum` value must be a double (e.g. 10, 24, etc.).
Please check again the value of `sitesNum`.\n----\n")
    allSiteEnvCharacts <- NULL
  }
}
