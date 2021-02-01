#' @title eLTER_getILTEREnvCharacts
#' @description This function ...
#' @param ...
#' @return The output of the function is a dataframe containing all the Environmental Characteristics of the all ILTER's sites.
#' @author Alessandro Oggioni, phD (2020) <oggioniale@gmail.com>
#' @import jsonlite
#' @export
#' @examples
#' getILTEREnvCharacts()
#'
### function getILTEREnvCharacts
getILTEREnvCharacts <- function() {
  lterILTERSites <- as.list(jsonlite::fromJSON("https://deims.org/api/sites"))
  allSiteEnvCharacts <- lapply(
    as.list(
      paste0(
        lterILTERSites$id$prefix,
        lterILTERSites$id$suffix
      )
    ),
    ReLTER::getSiteEnvCharacts
  )
  allSiteEnvCharacts_matrix <- do.call(rbind, allSiteEnvCharacts)
  allSiteEnvCharacts_df <- as_tibble(as.data.frame(allSiteEnvCharacts_matrix))
  allSiteEnvCharacts_df
}
