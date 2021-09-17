#' @title eLTER getNetworkSites function
#' @description This function provide a SpatialPointsDataFrame (sp) of sites in
#' a network provided by DEIMS-SDR.
#' @param networkDEIMSID A `character`. It is the DEIMS iD of network make from
#' DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}, and at this
#' \href{https://deims.org/search?f[0]=result_type:network}{page} the
#' complete list of iLTER networks.
#' @return The output of the function is `SpatialPointsDataFrame` (package sp)
#' of the network's sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import jsonlite sf
#' @export
#' @examples
#' listSites <- getNetworkSites(
#'   networkDEIMSID = "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
#' )
#' listSites[1:5, ]
#'
### function getNetworkSites
getNetworkSites <- function(networkDEIMSID) {
  lterNetworkSitesCoords <- jsonlite::fromJSON(
    paste0(
      "https://deims.org/",
      "api/sites?network=",
      substring(
        networkDEIMSID, 27
      )
    )
  )
  lterSitesNetworkPointDEIMS <- # sf::as_Spatial(
    sf::st_as_sf(lterNetworkSitesCoords, wkt = "coordinates")
    # )
  lterSitesNetworkPointDEIMS
}
