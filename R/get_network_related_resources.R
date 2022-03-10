#' Obtain information about the Related Resources (dataset and activity) of a
#' eLTER Network.
#' @description This function obtains the Related Resources information (title
#' and URL), as a stored in \href{https://deims.org/}{DEIMS-SDR catalogue}, of
#' all eLTER sites belonging to an eLTER Network (e.g.
#' \href{https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3}{LTER
#' Italy network}).
#' @param networkDEIMSID A `character`. It is the DEIMS.iD of network make
#' from DEIMS-SDR website. DEIMS.iD information 
#' \href{https://deims.org/docs/deimsid.html}{here} and Complete list of ILTER
#' networks \href{https://deims.org/search?f[0]=result_type:network}{here}.
#' The DEIMS.iD of network is the URL for the network page.
#' @return The output of the function is a `tibble` containing the related
#' resources shared by the network's sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct as_tibble
#' @export
#' @examples
#' \dontrun{
#' listRelatedResources <- get_network_related_resources(
#'   networkDEIMSID =
#'   "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
#' )
#' listRelatedResources[1:10, ]
#' }
#'
### function get_network_related_resources
get_network_related_resources <- function(networkDEIMSID) {
  lterNetworkSites <- as.list(
    jsonlite::fromJSON(
      paste0(
        "https://deims.org/",
        "api/sites?network=",
        sub("^.+/", "", networkDEIMSID)
      )
    )
  )
  allSiteRelatedResources <- lapply(
    as.list(
      paste0(
        lterNetworkSites$id$prefix[1:10],
        lterNetworkSites$id$suffix[1:10]
      )
    ),
    ReLTER::get_site_info,
    category = "RelateRes"
  )
  if (length(allSiteRelatedResources) != 0) {
    uniteSiteRelatedResources <- dplyr::bind_rows(allSiteRelatedResources)
    relatedResourcesNetworkList <- uniteSiteRelatedResources$relatedResources
    relatedResourcesNetworkDF <- dplyr::bind_rows(relatedResourcesNetworkList)
    relatedResourcesNetworkDF <- relatedResourcesNetworkDF %>%
      dplyr::select("relatedResourcesTitle", "uri", "relatedResourcesChanged")
    uniqueSiteRelatedResources <- dplyr::as_tibble(
      dplyr::distinct(
        relatedResourcesNetworkDF
      )
    )
    uniqueSiteRelatedResources
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the Network.iD\n----\n")
    uniqueSiteRelatedResources <- NULL
  }
}
