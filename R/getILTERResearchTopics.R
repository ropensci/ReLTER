#' @title eLTER getILTERResearchTopics function
#' @description This function allows to obtain the information about the
#' Research Topics collected in all sites of ILTER, through the DEIMS-SDR
#' sites API.
#' @param sitesNum A `integer`. It is the number of the sites that are 
#' read to get the information. Use this parameters moreover for provide
#' example of this function.
#' @return The output of the function is a `tibble` containing the research
#' topics and their URI (Uniform Resource Identifier) of all ILTER sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct as_tibble
#' @export
#' @examples
#' \dontrun{
#' listResearchTopics <- getILTERResearchTopics(sitesNum = 20)
#' listResearchTopics[1:10, ] %>%
#'   dplyr::rows_insert(
#'   dplyr::tibble(
#'     researchTopicsLabel = "...",
#'     researchTopicsUri = "..."
#'   )
#' )
#' }
#' 
### function getILTERResearchTopics
getILTERResearchTopics <- function(sitesNum = NULL) {
  require(dplyr)
  if (is.na(sitesNum)) {
    lterILTERSites <- as.list(jsonlite::fromJSON("https://deims.org/api/sites"))
    allSiteResearchTopics <- lapply(
      as.list(paste0(lterILTERSites$id$prefix, lterILTERSites$id$suffix)),
      ReLTER::getSiteResearchTopics
    )
    uniteSiteResearchTopics <- dplyr::bind_rows(allSiteResearchTopics)
    researchTopicsILTERList <- uniteSiteResearchTopics$researchTopics
    researchTopicsILTERDF <- dplyr::bind_rows(researchTopicsILTERList)
    uniqueSiteResearchTopics <- dplyr::as_tibble(
      dplyr::distinct(
        researchTopicsILTERDF
      )
    )
    uniqueSiteResearchTopics
  } else if (typeof(sitesNum) == "double") {
    lterILTERSites <- as.list(jsonlite::fromJSON("https://deims.org/api/sites"))
    allSiteResearchTopics <- lapply(
      as.list(paste0(
        lterILTERSites$id$prefix[1:sitesNum],
        lterILTERSites$id$suffix[1:sitesNum]
      )),
      ReLTER::getSiteResearchTopics
    )
    uniteSiteResearchTopics <- dplyr::bind_rows(allSiteResearchTopics)
    researchTopicsILTERList <- uniteSiteResearchTopics$researchTopics
    researchTopicsILTERDF <- dplyr::bind_rows(researchTopicsILTERList)
    uniqueSiteResearchTopics <- dplyr::as_tibble(
      dplyr::distinct(
        researchTopicsILTERDF
      )
    )
    uniqueSiteResearchTopics
  } else {
    message("\n---- The `sitesNum` value must be a double (e.g. 10, 24, etc.). Please check again the value of `sitesNum`. ----\n")
    uniqueSiteResearchTopics <- NULL
  }
}

# TODO: occorre mettere un controllo di errore tipo "status" nelle funzioni getSite...?
# In questo caso l'URL su cui viene fatta la chiamata non è parametrizzata e quindi
#   non credo che sia da inserire alcun controllo. Vero?