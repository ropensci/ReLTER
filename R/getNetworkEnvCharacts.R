#' @title eLTER_getNetworkEnvCharacts
#' @description This function ...
#' @param ...
#' @return The output of the function is a dataframe containing all the Environmental Characteristics of the network's sites.
#' @author Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
#' @import jsonlite ReLTER
#' @export
#' @examples
#' getNetworkEnvCharacts(networkDEIMSID = 'https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3')
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
  allSiteEnvCharacts_df <- as.data.frame(allSiteEnvCharacts_matrix)
  allSiteEnvCharacts_df
}

