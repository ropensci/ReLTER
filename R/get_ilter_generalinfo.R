#' Download information of all ILTER sites or a subset of ILTER sites.
#' @description `r lifecycle::badge("questioning")`
#' This function downloads generic information
#' of sites of
#' \href{https://www.ilter.network/network/global-coverage}{ILTER sites
#' (more than 1200 around the world)},
#' through the DEIMS-SDR API. 
#' Return a `tibble` object.
#' @param country_name A `character`. Country name (complete name in English, 
#' French, Italian, German, OR 2 character ISO code) of DEIMS sites
#' to retrieve. Partial matching of country names is NOT supported.
#' @param site_name A `character`. This character string filters by site name
#' where partial matching is supported.
#' At least one of country_name or site_name must be specified
#' @param show_map A `boolean`. If TRUE a Leaflet map of site locations
#' is shown. Default FALSE
#' @return An `sf` object of the bounding boxes of sites in the filtered list,
#' containing the name, DEIMS ID, longitude, latitude, average altitude,
#' and affiliation of the filtered ILTER sites.
#' If no bounding box is available,the centroid is returned.
#' @note at least one of `country_name` or `site_name` must be specified
#' @author Alessandro Oggioni, phD (2021) \email{oggioni.a@@irea.cnr.it}
#' @author  Micha Silver, phD (2021) \email{silverm@@post.bgu.ac.il}
#' @author  Paolo Tagliolato, phD (2023) \email{tagliolato.p@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_as_sf st_is_valid
#' @importFrom leaflet leaflet addTiles addMarkers
#' @references
#'   \insertRef{jsonliteR}{ReLTER}
#'
#'   \insertRef{sfR}{ReLTER}
#'
#'   \insertRef{leafletR}{ReLTER}
#' @export
#' @examples
#' \dontrun{
#' # list of the all sites info with ILTER
#' listOfAllSites <- get_ilter_generalinfo()
#' length(listOfAllSites[,1])
#'
#' # example about country name parameter
#' sitesAustria <- get_ilter_generalinfo(country_name = "Austri")
#' # (matches Austria, but not Australia)
#' length(sitesAustria$title)
#'
#' # example of single site in a country
#' eisenwurzen <- get_ilter_generalinfo(
#'   country_name = "Austri",
#'   site_name = "Eisen"
#' )
#' eisenwurzen[,1:2]
#' # extract DEIMS.Id
#' eisenwurzen_deimsid <- eisenwurzen$uri
#' eisenwurzen_deimsid
#'
#' # example of single site in a country and return only map
#' get_ilter_generalinfo(
#'   country_name = "Italy",
#'   site_name = "Maggiore",
#'   show_map = TRUE
#' )
#' }
### function get_ilter_generalinfo
get_ilter_generalinfo <- function(country_name = NA, site_name = NA,
                                  show_map = FALSE) {
  # if(all(is.na(c(country_name, site_name)))){
  #   warning("At least one of country_name or site_name must be specified.")
  #   return(NULL)
  # }
  country_code = NA
  
  if(!is.na(country_name)){
    if(nchar(country_name)>2) {
      country_code <- suppressWarnings({
        res<-countrycode::countrycode(country_name, origin = "country.name", 
                                      destination = "iso2c")
        res<-if(is.na(res)) 
          countrycode::countrycode(country_name, origin = "country.name.de", 
                                   destination = "iso2c") else res
        res<-if(is.na(res)) 
          countrycode::countrycode(country_name, origin = "country.name.it", 
                                   destination = "iso2c") else res
        res<-if(is.na(res)) 
          countrycode::countrycode(country_name, origin = "country.name.fr", 
                                   destination = "iso2c") else res
      })
    } else country_code <- country_name
  }
  
  if(all(is.na(c(country_code, site_name)))){
    warning("At least one valid country_name (complete name or 2 character ISO 
            code) or site_name must be specified.")
    return(NULL)
  }
  
  # Get full set of sites
  deimsbaseurl <- get_deims_base_url()
  url<-URLencode(paste0(
    deimsbaseurl,
    "api/sites?country=", country_code, "&name=", site_name
  ))
  
  lterILTERSites <- as.data.frame(jsonlite::fromJSON(url))
  
  if(nrow(lterILTERSites)==0){
    warning("Country and site name matched no ILTER site, 
            please check your request (country name must be complete or a 
            valid 2 character ISO code")
    return(NULL)
  }
  # Sites filtered by rest API
  filteredILTERSites <- lterILTERSites
  
  uniteSitesGeneralInfo <- filteredILTERSites %>% tibble::as_tibble() %>% 
    dplyr::mutate(uri = paste0(id$prefix, id$suffix), geoCoord=coordinates, 
                  .keep="unused", .before=changed)
    
  
  # # The following makes too many requests. 
  # # The code could be enabled only if sites are 
  # # less then a threshold.
  # # This is for maintaining some compatibility 
  # # (and for tests) but should be discarded.
  threshold_max_num_sites=3
  if(nrow(filteredILTERSites) < threshold_max_num_sites) {
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
  }
  
  # # The following is unnecessary, I put in comment. Should be deleted
  # Now filter by site name
  # valid_site <- (!is.na(site_name) & typeof(site_name) == "character")
  # if (valid_site) {
  #   idx <- grep(pattern = site_name,
  #               x = uniteSitesGeneralInfo$title,
  #               ignore.case = TRUE)
  #   if (length(idx) == 0) {
  #     warning(
  #       "\n",
  #       paste0(
  #         "You have provided a site name (\'",
  #         site_name,
  #         "\') that's not among the sites stored in the DEIMS-SDR.\n",
  #         "The result of this function will be
  #         all sites in the requested country (\'", country_name,
  #         "\') not a specific site.\n",
  #         "Please review what you entered in \'site_name\'."
  #       ),
  #       "\n"
  #     )
  #     uniteSitesGeneralInfo <- uniteSitesGeneralInfo
  #   } else {
  #     uniteSitesGeneralInfo <- uniteSitesGeneralInfo[idx, ]
  #   }
  # }
  # Make sure we have some rows
  if (is.null(uniteSitesGeneralInfo)) {
    return(NULL)
  } else if (length(uniteSitesGeneralInfo) == 0 |
             # No rows after country filter
             length(uniteSitesGeneralInfo$title) == 0) {
    # No rows left after site filter
    uniteSitesGeneralInfoGeo <- NULL
    warning("\n", paste("No matches found for country name:",
                        country_name,
                        "and site name:",
                        site_name
    ),
    "\n"
    )
    return(NULL)
  } else {
    # Now convert to sf object
    uniteSitesGeneralInfoGeo <- sf::st_as_sf(uniteSitesGeneralInfo,
                                             wkt = "geoCoord",
                                             crs = 4326)
    if (any(sf::st_is_valid(uniteSitesGeneralInfoGeo))) {
      if (show_map == TRUE) {
        map <- leaflet::leaflet(uniteSitesGeneralInfoGeo) %>%
          leaflet::addTiles() %>%
          leaflet::addMarkers(popup=uniteSitesGeneralInfoGeo$title)
        print(map)
      } else {
        return(uniteSitesGeneralInfoGeo)
      }
    } else {
      message("\n----\nThe map cannot be created because one or more site",
              " locations provided in DEIMS-SDR, has an invalid geometry.\n",
              "Please check the content and refer this error",
              " to DEIMS-SDR support.\n----\n")
      return(uniteSitesGeneralInfo)
    }
  }
}
