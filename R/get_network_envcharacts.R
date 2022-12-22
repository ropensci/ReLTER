#' Obtain a list of all Environmental Characteristics of sites in an
#' eLTER Network.
#' @description `r lifecycle::badge("stable")`
#' This function obtains all Environmental Characteristics:
#' title, URI, geo-coordinates, country name, and elevation
#' of eLTER Network sites (e.g.
#' \href{https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3}{LTER-
#' Italy network}), through the DEIMS-SDR API.
#' @param networkDEIMSID A `character`. DEIMS ID of network
#' from DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here} and Complete list of ILTER
#' networks \href{https://deims.org/search?f[0]=result_type:network}{here}.
#' The DEIMS ID of network is the URL for the network page.
#' @return The output of the function is a `tibble` containing all the
#' Environmental Characteristics of the network's sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @importFrom Rdpack reprompt
#' @importFrom purrr map_dfr
#' @references
#'   \insertRef{jsonliteR}{ReLTER}
#'
#'   \insertRef{dplyrR}{ReLTER}
#' @export
#' @examples
#' \dontrun{
#' listEnvCharacts <- get_network_envcharacts(
#'   networkDEIMSID =
#'     "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
#' )
#' listEnvCharacts[1:10, ]
#' }
#'
### function get_network_envcharacts
get_network_envcharacts <- function(networkDEIMSID) {
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
  allSiteEnvCharacts <- purrr::map_dfr(
    as.list(
      paste0(
        lterNetworkSites$id$prefix,
        lterNetworkSites$id$suffix
      )
    ),
    function (x) {
      ReLTER::get_site_info(x, category = "EnvCharacts")
    }
  )
  if (length(allSiteEnvCharacts) != 0) {
    allSiteEnvCharacts
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the Network.iD\n----\n")
    allSiteEnvCharacts <- NULL
  }
}
