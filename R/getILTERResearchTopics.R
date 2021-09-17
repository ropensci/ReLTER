#' @title eLTER getILTERResearchTopics function
#' @description This function allows to obtain the information about the
#' Research Topics collected in all sites of ILTER, througth the DEIMS-SDR
#' sites API.
#' @param null function witout parameter
#' @return The output of the function is a `tibble` containing the research
#' topics and their URI (Uniform Resource Identifier) of all ILTER sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import jsonlite dplyr
#' @export
#' @examples
#' require('dplyr')
#' listResearchTopics <- getILTERResearchTopics()
#' listResearchTopics[1:10, ] %>%
#'   dplyr::rows_insert(
#'   dplyr::tibble(
#'     researchTopicsLabel = "...",
#'     researchTopicsUri = "..."
#'   )
#' )
#'
### function getILTERResearchTopics
getILTERResearchTopics <- function() {
  lterILTERSites <- as.list(jsonlite::fromJSON("https://deims.org/api/sites"))
  allSiteResearchTopics <- lapply(
    as.list(paste0(lterILTERSites$id$prefix, lterILTERSites$id$suffix)),
    ReLTER::getSiteResearchTopics
  )
  uniteSiteResearchTopics <- dplyr::bind_rows(allSiteResearchTopics)
  researchTopicsILTERList <- uniteSiteResearchTopics$researchTopics
  researchTopicsILTERDF <- dplyr::bind_rows(researchTopicsILTERList)
  uniqueSiteResearchTopics <- tibble::as_tibble(
    dplyr::distinct(
      researchTopicsILTERDF
    )
  )
  uniqueSiteResearchTopics
}
