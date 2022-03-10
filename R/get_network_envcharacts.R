#' Obtain a list of all Environmental Characteristics of sites in an
#' eLTER Network.
#' @description This function obtains all Environmental Characteristics:
#' title, URI, geo-coordinates, country name, and elevation
#' of eLTER Network sites (e.g.
#' \href{https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3}{LTER-
#' Italy network}), through the DEIMS-SDR API.
#' @param networkDEIMSID A `character`. DEIMS.iD of network
#' from DEIMS-SDR website. DEIMS.iD information 
#' \href{https://deims.org/docs/deimsid.html}{here} and Complete list of ILTER
#' networks \href{https://deims.org/search?f[0]=result_type:network}{here}.
#' The DEIMS.iD of network is the URL for the network page.
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
