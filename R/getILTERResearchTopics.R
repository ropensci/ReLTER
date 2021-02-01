#' @title eLTER_getILTERResearchTopics
#' @description This function ...
#' @param ...
#' @return The output of the function is ...
#' @author Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
#' @import jsonlite ReLTER dplyr
#' @export
#' @examples
#' getILTERResearchTopics()
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
  uniqueSiteResearchTopics <- dplyr::distinct(researchTopicsILTERDF)
  uniqueSiteResearchTopics
}
