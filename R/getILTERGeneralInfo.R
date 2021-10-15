#' @title eLTER getILTERGeneralInfo function
#' @description This function allows to obtain the generic information
#' collected in all sites of ILTER, througth the DEIMS-SDR
#' sites API.
#' @param sitesNum A `integer`. It is the number of the sites that are 
#' read to get the information. Use this parameters oreover for provide
#' example of this function.
#' @return The output of the function is a `sf` containing the
#' name, longitude, latitude, average altitude, DEIMS.iD and affiliation
#' of all ILTER sites.
#' @author Alessandro Oggioni, phD (2021) \email{oggioni.a@@irea.cnr.it}
#' @import
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom sf st_as_sf
#' @examples
#' \donttest
#' require('dplyr')
#' listOfAllSites <- getILTERGeneralInfo(sitesNum = 20)
#' listOfAllSites[1:10, ]
#' \donttest
#' 
### function getILTERGeneralInfo
getILTERGeneralInfo <- function(sitesNum = NULL) {
  if (is.na(sitesNum)) {
    lterILTERSites <- as.list(
      jsonlite::fromJSON("https://deims.org/api/sites")
    )
    allSiteGeneralInfo <- lapply(
      as.list(paste0(lterILTERSites$id$prefix, lterILTERSites$id$suffix)),
      ReLTER::getSiteAffiliations
    )
    uniteSiteGeneralInfo <- dplyr::bind_rows(allSiteGeneralInfo)
    uniteSiteGeneralInfoGeo <- sf::st_as_sf(
      uniteSiteGeneralInfo, 
      wkt = "geoCoord", 
      crs = 4326
    )
    uniteSiteGeneralInfoGeo
  } else if (typeof(sitesNum) == "double") {
    lterILTERSites <- as.list(
      jsonlite::fromJSON("https://deims.org/api/sites")
    )
    allSiteGeneralInfo <- lapply(
      as.list(paste0(
        lterILTERSites$id$prefix[1:sitesNum],
        lterILTERSites$id$suffix[1:sitesNum]
      )),
      ReLTER::getSiteAffiliations
    )
    uniteSiteGeneralInfo <- dplyr::bind_rows(allSiteGeneralInfo)
    uniteSiteGeneralInfoGeo <- sf::st_as_sf(
      uniteSiteGeneralInfo,
      wkt = "geoCoord",
      crs = 4326
    )
    uniteSiteGeneralInfoGeo
  } else {
    message("\n---- The `sitesNum` value must be a double (e.g. 10, 24, etc.). Please check again the value of `sitesNum`. ----\n")
    uniteSiteGeneralInfoGeo <- NULL
  }
}

# TODO: occorre mettere un controllo di errore tipo "status" nelle funzioni getSite...?
# In questo caso l'URL su cui viene fatta la chiamata non è parametrizzata e quindi
#   non credo che sia da inserire alcun controllo. Vero?