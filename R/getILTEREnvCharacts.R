#' @title eLTER getILTEREnvCharacts function
#' @description This function allows to obtain the information about the
#' Environmental Characteristics of all sites of ILTER, througth the
#' DEIMS-SDR sites API.
#' @param null function witout parameter
#' @return The output of the function is a `tibble` containing all the
#' Environmental Characteristics of all ILTER's sites.
#' @author Alessandro Oggioni, phD (2020) <oggioni.a@@irea.cnr.it>
#' @import jsonlite
#' @export
#' @examples
#' listEnvCharacts <- getILTEREnvCharacts()
#' listEnvCharacts[1:5, ]
#'
### function getILTEREnvCharacts
getILTEREnvCharacts <- function() {
  lterILTERSites <- as.list(
    jsonlite::fromJSON("https://deims.org/api/sites")
  )
  allSiteEnvCharacts <- lapply(
    as.list(
      paste0(
        lterILTERSites$id$prefix[1:1217],
        lterILTERSites$id$suffix[1:1217]
      )
    ),
    ReLTER::getSiteEnvCharacts
  )
  allSiteEnvCharacts_matrix <- do.call(rbind, allSiteEnvCharacts)
  allSiteEnvCharacts <- tibble::as_tibble(
    as.data.frame(
      allSiteEnvCharacts_matrix
    )
  )
  allSiteEnvCharacts
}
