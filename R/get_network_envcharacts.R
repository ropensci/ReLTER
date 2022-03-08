#' @title Return a tibble object containing Environmental Characteristics,
#' stored in \href{https://deims.org/}{DEIMS-SDR catalogue}, of all the
#' eLTER sites belonging to an eLTER Network (e.g.
#' \href{https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3}{LTER
#' Italy network}).
#' @description This function obtains several Environmental Characteristics:
#' title, URI, geocoordinates, country name, and elevation
#' of sites in an eLTER Network (e.g. Italy) through the DEIMS-SDR sites API.
#' @param networkDEIMSID A `character`. DEIMS.iD of network
#' from DEIMS-SDR website. DEIMS.iD information 
#' \href{https://deims.org/docs/deimsid.html}{here} and Complete list of ILTER
#' networks \href{https://deims.org/search?f[0]=result_type:network}{here}.
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
