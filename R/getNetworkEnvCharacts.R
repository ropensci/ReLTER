#' @title eLTER getNetworkEnvCharacts function
#' @description This function allows to obtain the information about the
#' Envinronmental Characteristics of the eLTER Network (e.g. Italy) througth
#' the DEIMS-SDR sites API.
#' @param networkDEIMSID A `character`. It is the DEIMS iD of network make
#' from DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}, and at this
#' \href{https://deims.org/search?f[0]=result_type:network}{page}
#' the complete list of ILTER networks.
#' @return The output of the function is a `tibble` containing all the
#' Environmental Characteristics of the network's sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \donttest
#' listEnvCharacts <- getNetworkEnvCharacts(
#'   networkDEIMSID = "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
#' )
#' listEnvCharacts[1:10, ]
#' \donttest
#'
### function getNetworkEnvCharacts
getNetworkEnvCharacts <- function(networkDEIMSID) {
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
    ReLTER::getSiteEnvCharacts
  )
  if (length(allSiteEnvCharacts) != 0) {
    allSiteEnvCharacts_matrix <- do.call(rbind, allSiteEnvCharacts)
    allSiteEnvCharacts <- dplyr::as_tibble(allSiteEnvCharacts_matrix)
    allSiteEnvCharacts
  } else {
    message("\n---- The requested page could not be found. Please check again the Network.iD ----\n")
    allSiteEnvCharacts <- NULL
  }
}
