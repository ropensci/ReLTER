#' @title eLTER getNetworkParameters function
#' @description This function allows to obtain the information about the
#' parameters collected in the eLTER Network (e.g. Italy) througth the
#' DEIMS-SDR sites API.
#' @param networkDEIMSID A `character`. It is the DEIMS iD of network make
#' from DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}, and at this
#' \href{https://deims.org/search?f[0]=result_type:network}{page}
#' the complete list of ILTER networks.
#' @return The output of the function is a `tibble` containing the list
#' of parameters and their URI (Uniform Resource Identifier) collected
#' by the network's sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \donttest
#' require('dplyr')
#' listParams <- getNetworkParameters(
#'   networkDEIMSID = "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
#' )
#' listParams[1:10, ] %>%
#' dplyr::rows_insert(
#'   dplyr::tibble(parameterLabel = "...", parameterUri = "...")
#' )
#' \donttest
#'
### function getNetworkParameters
getNetworkParameters <- function(networkDEIMSID) {
  lterNetworkSites <- as.list(
    jsonlite::fromJSON(
      paste0("https://deims.org/",
             "api/sites?network=",
             sub("^.+/", "", networkDEIMSID)
            )
    )
  )
  allSiteParameters <- lapply(
    as.list(
      paste0(
        lterNetworkSites$id$prefix,
        lterNetworkSites$id$suffix
      )
    ),
    ReLTER::getSiteParameters
  )
  if (length(allSiteParameters) != 0) {
    uniteSiteParameters <- dplyr::bind_rows(allSiteParameters)
    parametersNetworkList <- uniteSiteParameters$parameter
    parametersNetworkDF <- dplyr::bind_rows(parametersNetworkList)
    uniqueSiteParameters <- dplyr::as_tibble(
      dplyr::distinct(
        parametersNetworkDF
      )
    )
    uniqueSiteParameters
  } else {
    message("\n---- The requested page could not be found. Please check again the Network.iD ----\n")
    uniqueSiteParameters <- NULL
  }
}
