#' @title eLTER get_network_related_resources function
#' @description This function obtains information about the
#' related resources (e.g. dataset) shared in the eLTER Network (e.g. Italy)
#' through the DEIMS-SDR sites API.
#' @param networkDEIMSID A `character`. It is the DEIMS iD of network make
#' from DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}, and at this
#' \href{https://deims.org/search?f[0]=result_type:network}{page} the
#' complete list of ILTER networks.
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
