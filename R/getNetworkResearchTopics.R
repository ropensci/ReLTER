#' @title eLTER getNetworkResearchTopics function
#' @description This function allows to obtain the information about the
#' Research Topics collected in the eLTER Network (e.g. Italy) througth
#' the DEIMS-SDR sites API.
#' @param networkDEIMSID A `character`. It is the DEIMS iD of network make
#' from DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}, and at this
#' \href{https://deims.org/search?f[0]=result_type:network}{page} the
#' complete list of ILTER networks.
#' @return The output of the function is a `tibble` containing the research
#' topics and their URI (Uniform Resource Identifier) collected by network's
#' sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct as_tibble
#' @export
#' @examples
#' \dontrun{
#' require('dplyr')
#' listResearchTopics <- getNetworkResearchTopics(
#'   networkDEIMSID = "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
#' )
#' listResearchTopics[1:10, ] %>%
#'   dplyr::rows_insert(
#'     dplyr::tibble(
#'       researchTopicsLabel = "...",
#'       researchTopicsUri = "..."
#'     )
#'   )
#' }
#'
### function getNetworkResearchTopics
getNetworkResearchTopics <- function(networkDEIMSID) {
  lterNetworkSites <- as.list(
    jsonlite::fromJSON(
      paste0(
        "https://deims.org/",
        "api/sites?network=",
        sub("^.+/", "", networkDEIMSID)
      )
    )
  )
  allSiteResearchTopics <- lapply(
    as.list(
      paste0(
        lterNetworkSites$id$prefix,
        lterNetworkSites$id$suffix
      )
    ),
    ReLTER::getSiteResearchTopics
  )
  if (length(allSiteResearchTopics) != 0) {
    uniteSiteResearchTopics <- dplyr::bind_rows(allSiteResearchTopics)
    researchTopicsNetworkList <- uniteSiteResearchTopics$researchTopics
    researchTopicsNetworkDF <- dplyr::bind_rows(researchTopicsNetworkList)
    uniqueSiteResearchTopics <- dplyr::as_tibble(
      dplyr::distinct(
        researchTopicsNetworkDF
      )
    )
    uniqueSiteResearchTopics
  } else {
    message("\n---- The requested page could not be found. Please check again the Network.iD ----\n")
    uniqueSiteParameters <- NULL
  }
}
