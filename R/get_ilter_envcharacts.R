#' Obtain a list of all Environmental Characteristics of ILTER sites.
#' @description This function obtains all Environmental Characteristics:
#' title, URI, geo-coordinates, country name, and elevation
#' of all
#' \href{https://www.ilter.network/network/global-coverage}{ILTER sites
#' (more than 1200 around the world)},
#' through the DEIMS-SDR API.
#' @param sitesNum A `integer`. It is the number of the sites that are
#' read to get the information. Use this parameters moreover for provide
#' example of this function.
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
