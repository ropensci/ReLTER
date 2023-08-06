#' Obtain information about the Related Resources (dataset and activity) of a
#' eLTER Network.
#' @description `r lifecycle::badge("stable")`
#' This function obtains the Related Resources information (title
#' and URL), as a stored in \href{https://deims.org/}{DEIMS-SDR catalogue}, of
#' all eLTER sites belonging to an eLTER Network (e.g.
#' \href{https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3}{LTER
#' Italy network}).
#' @param networkDEIMSID A `character`. It is the DEIMS ID of network make
#' from DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here} and Complete list of ILTER
#' networks \href{https://deims.org/search?f[0]=result_type:network}{here}.
#' The DEIMS ID of network is the URL for the network page.
#' @return The output of the function is a `tibble` containing the related
#' resources shared by the network's sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct as_tibble
#' @importFrom Rdpack reprompt
#' @references
#'   \insertRef{jsonliteR}{ReLTER}
#'
#'   \insertRef{dplyrR}{ReLTER}
#' @export
#' @examples
#' \dontrun{
#' listRelatedResources <- get_network_related_resources(
#'   networkDEIMSID =
#'   "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
#' )
#' listRelatedResources
#' }
#'
### function get_network_related_resources
get_network_related_resources <- function(networkDEIMSID) {
  deimsbaseurl <- get_deims_base_url()
  lterNetworkSites <- as.list(
    jsonlite::fromJSON(
      paste0(
        deimsbaseurl,
        "api/sites?network=",
        sub("^.+/", "", networkDEIMSID)
      )
    )
  )
  allSiteRelatedResources <- lapply(
    as.list(
      paste0(
        lterNetworkSites$id$prefix,
        lterNetworkSites$id$suffix
      )
    ),
    ReLTER::get_site_info,
    category = "RelateRes"
  )
  
  if (length(allSiteRelatedResources) != 0) {
    uniteSiteRelatedResources <- dplyr::bind_rows(allSiteRelatedResources)
    #relatedResourcesNetworkList <- uniteSiteRelatedResources$relatedResources
    #relatedResourcesNetworkDF <- dplyr::bind_rows(relatedResourcesNetworkList)
    relatedResourcesNetworkDF <- 
      dplyr::bind_rows(uniteSiteRelatedResources$relatedResources)
    if("uri" %in% colnames(relatedResourcesNetworkDF)){
      relatedResourcesNetworkDF <- relatedResourcesNetworkDF %>%
        dplyr::select("relatedResourcesTitle", "uri", "relatedResourcesChanged")
      uniqueSiteRelatedResources <- dplyr::as_tibble(
        dplyr::distinct(
          relatedResourcesNetworkDF
        )
      )
      return(uniqueSiteRelatedResources)
    }
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the Network.iD\n----\n")
    uniqueSiteRelatedResources <- NULL
  }
}
