#' @title eLTER_getNetworkResearchTopics
#' @description This function ...
#' @param networkDEIMSID is a DEIMS iD of network make from DEIMS-SDR website. More information about DEIMS iD in this page https://deims.org/docs/deimsid.html, and at this page https://deims.org/search?f%5B0%5D=result_type%3Anetwork the complete list of iLTER networks.
#' @return The output of the function is ...
#' @author Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
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
  uniqueSiteResearchTopics <- dplyr::distinct(researchTopicsNetworkDF)
  uniqueSiteResearchTopics
}
