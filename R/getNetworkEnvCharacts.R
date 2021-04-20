#' @title eLTER getNetworkEnvCharacts function
#' @description This function allows to obtain the information about the Envinronmental Characteristics of the eLTER Network (e.g. Italy) througth the DEIMS-SDR sites API.
#' @param  networkDEIMSID A `character`. It is the DEIMS iD of network make from DEIMS-SDR website. More information about DEIMS iD in this \href{https://deims.org/docs/deimsid.html}{page}, and at this \href{https://deims.org/search?f%5B0%5D=result_type%3Anetwork}{page} the complete list of ILTER networks.
#' @return The output of the function is a `tibble` containing all the Environmental Characteristics of the network's sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import jsonlite
#' @export
#' @examples
#' require('dplyr')
#' listEnvCharacts <- getNetworkEnvCharacts(networkDEIMSID = 'https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3')
#' listEnvCharacts[1:5, ]
#'
### function getNetworkEnvCharacts
getNetworkEnvCharacts <- function(networkDEIMSID) {
  lterNetworkSites <- as.list(jsonlite::fromJSON(paste0("https://deims.org/", "api/sites?network=", substring(networkDEIMSID, 27))))
  allSiteEnvCharacts <- lapply(
    as.list(
      paste0(
        lterNetworkSites$id$prefix,
        lterNetworkSites$id$suffix
      )
    ),
    ReLTER::getSiteEnvCharacts
  )
  allSiteEnvCharacts_matrix <- do.call(rbind, allSiteEnvCharacts)
  allSiteEnvCharacts <- tibble::as_tibble(allSiteEnvCharacts_matrix)
  allSiteEnvCharacts
}
