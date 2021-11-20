#' @title eLTER get_ilter_generalinfo function
#' @description This function downloads generic information
#' of all sites, or a subset of sites of ILTER, through the DEIMS-SDR
#' API. If no `country_name` or `site_name` are specified,
#' the whole list of sites is returned. If either or both of the filtering
#' strings is specified, then a filtered subset of the sites is acquired.
#' @param country_name A `character`. This character string filters 
#' the full set of DEIMS sites by country name. Partial matching is supported.
#' @param site_name A `character`. This character string filters by site name
#' where, again, partial matching is supported
#' @param show_map A `boolean`. If TRUE a Leaflet map of site locations 
#' is shown. Default FALSE
#' @return An `sf` object of the bounding boxes of sites in the filtered list,
#' containing the name, DEIMS ID, longitude, latitude, average altitude,
#' and affiliation of the filtered ILTER sites. 
#' If no bounding box is available,the centroid is returned.
#' @author Alessandro Oggioni, phD (2021) \email{oggioni.a@@irea.cnr.it}
#' @author  Micha Silver, phD (2021) \email{silverm@@post.bgu.ac.il}
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_as_sf
#' @export
#' @examples
#' \dontrun{
#' listOfAllSites <- get_ilter_generalinfo()
#' length(listOfAllSites[,1])
#' sitesAustria <- get_ilter_generalinfo(country_name = "Austri")
#' # (matches Austria, but not Australia)
#' length(sitesAustria$title)
#' eisenwurzen <- get_ilter_generalinfo(country_name = "Austri",
#'                                      site_name =" Eisen")
#' eisenwurzen[,1:2]
#' eisenwurzen_deimsid <- eisenwurzen$uri
#' eisenwurzen_deimsid
#' }
### function get_ilter_generalinfo
get_ilter_generalinfo <- function(country_name = NA, site_name = NA,
                                  show_map = FALSE) {
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
    if (length(idx) == 0) {
      warning(
        "\n",
        paste0(
          "You have provided a country name (\'",
          country_name,
          "\') that is not among the countries stored in the DEIMS-SDR.",
          " Please review what you entered in \'country_name\'.\n"
        ),
        "\n"
      )
      filteredILTERSites <- NULL
    } else {
      filteredILTERSites <- lterILTERSites[idx, ]
    }
  } else {
    # No country filtering
    filteredILTERSites <- lterILTERSites
  }
  # Now get affiliations, general info
  filteredSitesGeneralInfo <- lapply(
    as.list(
      paste0(
        filteredILTERSites$id$prefix,
        filteredILTERSites$id$suffix
      )
    ),
    ReLTER::get_site_info,
    category = "Affiliations"
  )
  uniteSitesGeneralInfo <- do.call(rbind, filteredSitesGeneralInfo)
  # Now filter by site name
  if (!is.na(site_name) & typeof(site_name) == "character") {
    idx <- grep(pattern = site_name,
                x = uniteSitesGeneralInfo$title,
                ignore.case = TRUE)
    if (length(idx) == 0) {
      warning(
        "\n",
        paste0(
          "You have provided a site name (\'",
          site_name,
          "\') that's not among the sites stored in the DEIMS-SDR.\n",
          "The result of this function will be 
          all sites in the requested country (\'", country_name,
          "\') not a specific site.\n",
          "Please review what you entered in \'site_name\'."
        ),
        "\n"
      )
      uniteSitesGeneralInfo <- uniteSitesGeneralInfo
    } else{
      uniteSitesGeneralInfo <- uniteSitesGeneralInfo[idx, ]
    }
  }
  # Make sure we have some rows
  if (is.null(uniteSitesGeneralInfo)) {
    uniteSitesGeneralInfoGeo <- NULL
    uniteSitesGeneralInfoGeo
  } else if (length(uniteSitesGeneralInfo[,1]) == 0 |
          # No rows after country filter
          length(uniteSitesGeneralInfo$title) == 0) {
          # No rows left after site filter
          uniteSitesGeneralInfoGeo <- NULL
       warning(
      "\n" ,
      paste(
        "No matches found for country name:",
        country_name,
        "and site name:",
        site_name
      ),
      "\n"
    )
    uniteSitesGeneralInfoGeo
  } else {
    # Now convert to sf object
    uniteSitesGeneralInfoGeo <- sf::st_as_sf(uniteSitesGeneralInfo,
                                             wkt = "geoCoord",
                                             crs = 4326)
    uniteSitesGeneralInfoGeo_SP <- sf::as_Spatial(
      uniteSitesGeneralInfoGeo$geoCoord
    )
    uniteSitesGeneralInfoGeo_valid <- rgeos::gIsValid(
      uniteSitesGeneralInfoGeo_SP,
      byid = FALSE,
      reason = TRUE
    )
    if (uniteSitesGeneralInfoGeo_valid == "Valid Geometry") {
      if (show_map == TRUE) {
        map <- leaflet::leaflet(uniteSitesGeneralInfoGeo) %>%
          leaflet::addTiles() %>%
          leaflet::addMarkers()
        print(map)
      }
      uniteSitesGeneralInfoGeo
    } else {
      message("\n----\nThe map cannot be created because one or more site",
             " locations provided in DEIMS-SDR, has an invalid geometry.\n",
             "Please check the content and refer this error",
             " to DEIMS-SDR support.\n----\n")
      uniteSitesGeneralInfoGeo
    }
  }
}
