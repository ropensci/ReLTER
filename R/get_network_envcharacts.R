#' @title eLTER get_network_envcharacts function
#' @description This function obtains several Envinronmental Characteristics:
#' title, URI, geocoordinates, country name, and elevation
#' of sites in an eLTER Network (e.g. Italy) through the DEIMS-SDR sites API.
#' @param networkDEIMSID A `character`. DEIMS ID of network
#' from DEIMS-SDR website. More information about DEIMS ID refer to these pages:
#' \href{https://deims.org/docs/deimsid.html}{page}, and
#' \href{https://deims.org/search?f[0]=result_type:network}{page}
#' the complete list of ILTER networks.
#' @return The output of the function is a `tibble` containing all the
#' Environmental Characteristics of the network's sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \dontrun{
#' listEnvCharacts <- get_network_envcharacts(
#'   networkDEIMSID =
#'   "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
#' )
#' listEnvCharacts[1:10, ]
#' }
#'
### function get_network_envcharacts
get_network_envcharacts <- function(networkDEIMSID) {
  lterNetworkSites <- as.list(
    jsonlite::fromJSON(
      paste0(
        "https://deims.org/",
        "api/sites?network=",
        sub("^.+/", "", networkDEIMSID)
      )
    )
  )
  allSiteEnvCharacts <- lapply(
    as.list(
      paste0(
        lterNetworkSites$id$prefix,
        lterNetworkSites$id$suffix
      )
    ),
    ReLTER::get_site_info,
    category = "EnvCharacts"
  )
  if (length(allSiteEnvCharacts) != 0) {
    allSiteEnvCharacts_matrix <- do.call(rbind, allSiteEnvCharacts)
    allSiteEnvCharacts <- dplyr::as_tibble(allSiteEnvCharacts_matrix)
    allSiteEnvCharacts
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the Network.iD\n----\n")
    allSiteEnvCharacts <- NULL
  }
}
