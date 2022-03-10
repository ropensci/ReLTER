#' Obtain a list of all the Environmental Parameters of sites in an
#' eLTER Network.
#' @description This function obtains all Environmental Parameters
#' collected in an eLTER Network (e.g.
#' \href{https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3}{LTER-
#' Italy network}), through the DEIMS-SDR API.
#' @param networkDEIMSID A `character`. The DEIMS.iD of network
#' from DEIMS-SDR website. DEIMS.iD information 
#' \href{https://deims.org/docs/deimsid.html}{here} and Complete list of ILTER
#' networks \href{https://deims.org/search?f[0]=result_type:network}{here}.
#' The DEIMS.iD of network is the URL for the network page.
#' @return The output of the function is a `tibble` containing the list
#' of parameters and their URI (Uniform Resource Identifier) collected
#' by the network's sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \dontrun{
#' listParams <- get_network_parameters(
#'   networkDEIMSID =
#'   "https://deims.org/networks/e0f680c2-22b1-4424-bf54-58aa9b7476a0"
#' )
#' listParams[1:10, ] %>%
#' dplyr::rows_insert(
#'   dplyr::tibble(parameterLabel = "...", parameterUri = "...")
#' )
#' }
#'
### function get_network_parameters
get_network_parameters <- function(networkDEIMSID) {
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
    ReLTER::get_site_info,
    category = "Parameters"
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
    message("\n----\nThe requested page could not be found.
Please check again the Network.iD\n----\n")
    uniqueSiteParameters <- NULL
  }
}
