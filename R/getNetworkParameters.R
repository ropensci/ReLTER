#' @title eLTER_getNetworkParameters
#' @description This function ...
#' @param ...
#' @return The output of the function is ...
#' @author Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
#' @import jsonlite ReLTER dplyr
#' @export
#' @examples
#' getNetworkParameters(networkDEIMSID = 'https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3')
#'
### function getNetworkParameters
getNetworkParameters <- function(networkDEIMSID) {
  lterNetworkSites <- as.list(jsonlite::fromJSON(paste0("https://deims.org/", "api/sites?network=", substring(networkDEIMSID, 27))))
  allSiteParameters <- lapply(
    as.list(
      paste0(
        lterNetworkSites$id$prefix,
        lterNetworkSites$id$suffix
      )
    ),
    ReLTER::getSiteParameters
  )
  uniteSiteParameters <- dplyr::bind_rows(allSiteParameters)
  parametersNetworkList <- uniteSiteParameters$parameter
  parametersNetworkDF <- dplyr::bind_rows(parametersNetworkList)
  uniqueSiteParameters <- dplyr::distinct(parametersNetworkDF)
  uniqueSiteParameters
}
