#' @title eLTER getILTEREnvCharacts function
#' @description This function obtains the information about the
#' Environmental Characteristics of ILTER sites, through the DEIMS-SDR sites API.
#' @param sitesNum A `integer`. It is the number of the sites that are 
#' read to get the information. Use this parameters moreover for provide
#' example of this function.
#' @return The output of the function is a `tibble` containing all the
#' Environmental Characteristics of ILTER's sites.
#' @author Alessandro Oggioni, phD (2020) <oggioni.a@@irea.cnr.it>
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \dontrun{
#' listEnvCharacts <- getILTEREnvCharacts(sitesNum = 10)
#' listEnvCharacts[1:20, ]
#' }
#' 
### function getILTEREnvCharacts
getILTEREnvCharacts <- function(sitesNum = NULL) {
  if (is.na(sitesNum)) {
    lterILTERSites <- as.list(
      jsonlite::fromJSON("https://deims.org/api/sites")
    )
    allSiteEnvCharacts <- lapply(
      as.list(
        paste0(
          lterILTERSites$id$prefix,
          lterILTERSites$id$suffix
        )
      ),
      getSiteEnvCharacts
    )
    allSiteEnvCharacts_matrix <- do.call(rbind, allSiteEnvCharacts)
    allSiteEnvCharacts <- dplyr::as_tibble(
      as.data.frame(
        allSiteEnvCharacts_matrix
      )
    )
    ReLTER::allSiteEnvCharacts
  } else if (typeof(sitesNum) == "double") {
    lterILTERSites <- as.list(
      jsonlite::fromJSON("https://deims.org/api/sites")
    )
    allSiteEnvCharacts <- lapply(
      as.list(
        paste0(
          lterILTERSites$id$prefix[1:sitesNum],
          lterILTERSites$id$suffix[1:sitesNum]
        )
      ),
      ReLTER::getSiteEnvCharacts
    )
    allSiteEnvCharacts_matrix <- do.call(rbind, allSiteEnvCharacts)
    allSiteEnvCharacts <- dplyr::as_tibble(
      as.data.frame(
        allSiteEnvCharacts_matrix
      )
    )
    allSiteEnvCharacts
  } else {
    message("\n---- The `sitesNum` value must be a double (e.g. 10, 24, etc.). Please check again the value of `sitesNum`. ----\n")
    allSiteEnvCharacts <- NULL
  }
}
