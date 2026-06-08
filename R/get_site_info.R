#' Obtain details about an eLTER site.
#' @description `r lifecycle::badge("stable")`
#' This function obtains information of a single eLTER site,
#' as a stored in \href{https://deims.org/}{DEIMS-SDR catalogue},
#' through the DEIMS-SDR API.
#' @param categories A `categories`. This parameter selects which categories
#' or categories are retrieved and returned in the result.
#' Possible value are:
#' 'Affiliations', 'Contacts', 'EnvCharacts', 'General',
#' 'Infrastructure', 'observedProperties', 'RelateRes'.
#' Multiple values can be indicated.
#' A site's boundary is always returned.
#' @param show_map A `boolean`. When TRUE a `leaflet` map is plotted as side
#' effect. Default FALSE.
#' @param with_locations A `boolean`. When TRUE, and only `show_map` is TRUE,
#' all site related locations are showed in the plotted map.
#' Default FALSE.
#' @inheritParams get_site_boundaries
#' @return The output of the function is a `sf` with the information about
#' the site.
#' If the boundary is missing from DEIMS-SDR a `tibble` is returned.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @importFrom dplyr as_tibble left_join
#' @importFrom lubridate as_datetime
#' @importFrom units set_units
#' @importFrom utils capture.output
#' @importFrom sf st_as_sf
#' @export
#' @examples
#' site <- get_site_info(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
#'   categories = c("EnvCharacts", "Affiliations"),
#'   show_map = TRUE,
#'   with_locations = FALSE
#' )
#' site
#' 
### function get_site_info
get_site_info <- function(
    deimsid,
    categories = NA,
    show_map = FALSE,
    with_locations = FALSE
) {
  if (isFALSE(show_map)) {
    with_locations <- FALSE
  }
  
  qo <- queries_jq_deims[[get_deims_API_version()]]$site_info
  siteInfo <- .materialise_query(qo, deimsid, "site_info")
  
  if (is.null(siteInfo) || nrow(siteInfo) == 0L) {
    message("\n----\nThe requested page could not be found.",
            "\nPlease check the DEIMS ID: ", deimsid, "\n----\n")
    return(invisible(NULL))
  }
  
  # Flatten country from list-column to vector
  siteInfo$country <- unlist(siteInfo$country)
  
  # Set elevation units [m]
  elev_cols <- c("geoElev.avg", "geoElev.min", "geoElev.max")
  siteInfo[elev_cols] <- lapply(siteInfo[elev_cols], units::set_units, value = "m")
  
  # Parse created and changed as datetime
  siteInfo$created <- lubridate::as_datetime(siteInfo$created)
  siteInfo$changed <- lubridate::as_datetime(siteInfo$changed)
  
  # --- Optional categories ---
  category_map <- list(
    Affiliations = function() get_site_affiliations(deimsid),
    Contacts = function() get_site_contact(deimsid),
    EnvCharacts = function() get_site_envcharacts(deimsid),
    General = function() get_site_general(deimsid),
    Infrastructure = function() get_site_infrastructure(deimsid),
    observedProperties = function() get_site_observedProperties(deimsid),
    RelateRes = function() get_site_related_resources(deimsid)
  )
  
  join_keys <- c(
    "title", "uri", "geoCoord", "country",
    "geoElev.avg", "geoElev.min", "geoElev.max", "geoElev.unit"
  )
  
  if (any(!is.na(categories))) {
    for (cat in names(category_map)) {
      if (any(grepl(cat, categories))) {
        extra <- category_map[[cat]]()
        if (!is.null(extra)) {
          siteInfo <- dplyr::left_join(siteInfo, extra, by = join_keys)
        }
      }
    }
  }
  
  # --- Boundaries ---
  bound <- get_site_boundaries(
    deimsid = deimsid,
    show_map = show_map,
    with_locations = with_locations
  )
  
  if (!is.null(bound) && inherits(bound$data, "sf")) {
    siteInfo <- siteInfo |>
      dplyr::left_join(bound$data, by = "uri") |>
      sf::st_as_sf(sf_column_name = "geometry")
  } else {
    message("\n----\nThe requested DEIMS-SDR site doesn't contain",
            "\ngeographic boundary information.",
            "\nA simple tibble is returned and no map is shown.\n----\n")
  }
  
  siteInfo
}
