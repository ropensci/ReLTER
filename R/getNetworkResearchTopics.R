#' @title eLTER_getNetworkResearchTopics
#' @description This function allows to obtain the information about the Research Topics collected in the eLTER Network  (e.g. Italy) througth the DEIMS-SDR sites API.
#' @param  networkDEIMSID A `character`. It is the DEIMS iD of network make from DEIMS-SDR website.
#'  More information about DEIMS iD in this \href{https://deims.org/docs/deimsid.html}{page}, and at this \href{https://deims.org/search?f%5B0%5D=result_type%3Anetwork}{page} the complete 
#'  list of ILTER networks.
#' @return The output of the function is a `tibble` containing the research topics and their URI (Uniform Resource Identifier) collected by network's sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import jsonlite ReLTER dplyr
#' @export
#' @examples
#' listResearchTopics <- getNetworkResearchTopics(networkDEIMSID = 'https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3')
#' knitr::kable(listResearchTopics[1:10, ] %>% dplyr::rows_insert(tibble(researchTopicsLabel = "...", researchTopicsUri = "...")))
#'
### function getNetworkResearchTopics
getNetworkResearchTopics <- function(networkDEIMSID) {
  lterNetworkSites <- as.list(jsonlite::fromJSON(paste0("https://deims.org/", "api/sites?network=", substring(networkDEIMSID, 27))))
  allSiteResearchTopics <- lapply(
    as.list(paste0(lterNetworkSites$id$prefix, lterNetworkSites$id$suffix)),
    ReLTER::getSiteResearchTopics
  )
  uniteSiteResearchTopics <- dplyr::bind_rows(allSiteResearchTopics)
  researchTopicsNetworkList <- uniteSiteResearchTopics$researchTopics
  researchTopicsNetworkDF <- dplyr::bind_rows(researchTopicsNetworkList)
  uniqueSiteResearchTopics <- tibble::as_tibble(dplyr::distinct(researchTopicsNetworkDF))
  uniqueSiteResearchTopics
}
