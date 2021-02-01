#' @title eLTER_getNetworkRelatedResources
#' @description This function ...
#' @param ...
#' @return The output of the function is ...
#' @author Alessandro Oggioni, phD (2020) <oggioniale@gmail.com>
#' @import jsonlite
#' @export
#' @examples
#' getNetworkRelatedResources(networkDEIMSID = 'https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3')
#'
### function getNetworkRelatedResources
getNetworkRelatedResources <- function(networkDEIMSID) {
  lterNetworkSites <- as.list(jsonlite::fromJSON(paste0("https://deims.org/", "api/sites?network=", substring(networkDEIMSID, 27))))
  allSiteRelatedResources <- lapply(
    as.list(
      paste0(
        lterNetworkSites$id$prefix,
        lterNetworkSites$id$suffix
      )
    ),
    ReLTER::getSiteRelatedResources
  )
  uniteSiteRelatedResources <- dplyr::bind_rows(allSiteRelatedResources)
  relatedResourcesNetworkList <- uniteSiteRelatedResources$relatedResources
  relatedResourcesNetworkDF <- dplyr::bind_rows(relatedResourcesNetworkList)
  uniqueSiteRelatedResources <- dplyr::distinct(relatedResourcesNetworkDF)
  uniqueSiteRelatedResources
}
