#' @title eLTER getILTERGeneralInfo function
#' @description This function downloads generic information
#' of all sites, or a subset of sites of ILTER, through the DEIMS-SDR
#' API. If no `country_name` or `site_name` are specified, 
#' the whole list of sites is returned. If either or both of the filtering strings
#' is specified, then a filtered subset of the sites is acquired. 
#' @param country_name A `character`. This character string filters the full set
#' of DEIMS sites by country name. Partial matching is supported.
#' @param site_name A `character`. This character string filters by site name
#' where, again, partial matching is supported
#' @return An `sf` object of the bounding boxes of sites in the filtered list,
#' containing the name, DEIMS ID, longitude, latitude, average altitude, 
#' and affiliation of the filtered ILTER sites. If no bounding box is available,
#' the centroid is returned.
#' @author Alessandro Oggioni, phD (2021) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom sf st_as_sf
#' @export
#' @examples
#' \dontrun{
#' require('dplyr')
#' listOfAllSites <- getILTERGeneralInfo()
#' length(listOfAllSites$geom)
#' sitesAustria <- getILTERGeneralInfo(country_name = "Austri")
#' # (matches Austria, but not Australia)
#' length(sitesAustria$title)
#' eisenwurzen <- getILTERGeneralInfo(country_name="Austri", site_name="Eisen")
#' eisenwurzen[,1:2]
#' eisenwurzen_deimsid <- eisenwurzen$uri
#' \donttest
#' 
### function getILTERGeneralInfo
getILTERGeneralInfo <- function(country_name = NA, site_name = NA) {
  # Get full set of sites
  lterILTERSites <- as.data.frame(
      jsonlite::fromJSON("https://deims.org/api/sites")
  )
  # First filter by country_name 
  # (Getting site affiliations for all 1200 sites takes too long...)
  if (!is.na(country_name) & typeof(country_name) == "character") {
    idx <- grep(x = lterILTERSites$title,
                pattern = country_name,
                ignore.case = TRUE)
    filteredILTERSites <- lterILTERSites[idx,]
  } else {
    # No country filtering
    filteredILTERSites <- lterILTERSites
  }
  # Now get affiliations, general info
  filteredSitesGeneralInfo <- lapply(
      as.list(paste0(filteredILTERSites$id$prefix,
                     filteredILTERSites$id$suffix)),
      ReLTER::getSiteAffiliations
  )
  uniteSitesGeneralInfo <- do.call(rbind, filteredSitesGeneralInfo)
  
  # Now filter by site name
  if (!is.na(site_name) & typeof(site_name) == "character") {
    idx <- grep(pattern = site_name,
                x=uniteSitesGeneralInfo$title, ignore.case = TRUE)
    uniteSitesGeneralInfo <- uniteSitesGeneralInfo[idx,]
  }
  
  # Make sure we have some rows
  if (length(uniteSitesGeneralInfo) == 0 | # No rows after country filter
      length(uniteSitesGeneralInfo$title) == 0) { # No rows left after site filter
    warning("\n" ,paste("No matches found for country name:",
                      country_name, "and site name:", site_name), 
            "\n")
    return(NA)
  } else {
    # Now convert to sf object
    uniteSitesGeneralInfoGeo <- sf::st_as_sf(
      uniteSitesGeneralInfo, 
      wkt = "geoCoord", 
      crs = 4326
    )
  } 

  return(uniteSitesGeneralInfoGeo)
}

# TODO: occorre mettere un controllo di errore tipo "status" nelle funzioni getSite...?
# In questo caso l'URL su cui viene fatta la chiamata non è parametrizzata e quindi
#   non credo che sia da inserire alcun controllo. Vero?
