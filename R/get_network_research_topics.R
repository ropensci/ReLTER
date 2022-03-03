#' @title Return a tibble object containing Research Topics collected by all
#' of the eLTER sites belonging to an eLTER Network (e.g.
#' \href{https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3}{LTER
#' Italy network}), as a stored into \href{https://deims.org}{DEIMS-SDR}.
#' @description This function obtains a list of Research Topics
#' handled in the eLTER Network (e.g. Italy) through
#' the DEIMS-SDR sites API.
#' @param networkDEIMSID A `character`. The DEIMS ID of a network
#' from DEIMS-SDR website. More information about DEIMS network ID in these
#' pages: \href{https://deims.org/docs/deimsid.html}{page}, and
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
#' listResearchTopics <- get_network_research_topics(
#'   networkDEIMSID =
#'   "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
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
### function get_network_research_topics
get_network_research_topics <- function(networkDEIMSID) {
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
    ReLTER::get_site_info,
    category = "ResearchTop"
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
    message("\n----\nThe requested page could not be found.
Please check again the Network.iD\n----\n")
    uniqueSiteResearchTopics <- NULL
  }
}
