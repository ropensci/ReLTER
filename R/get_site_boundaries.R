#' eLTER get_site_boundaries function
#' @description `r lifecycle::badge("stable")`
#' This internal function retrieves the boundary of a specified eLTER site
#' and a view of the site boundaries on a leaflet map is shown.
#' All the info are taken from the DEIMS-SDR sites API.
#' If the boundary is missing, a warning message is printed in the R console.
#' @param deimsid A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @param show_map A `boolean` or the string "return". When TRUE a `leflet`
#' object (a map) is returned and plotted. Default FALSE.
#' When the value is "return", the `leaflet` object is returned but not
#' plotted.
#' @param with_locations A `boolean`. When TRUE all site boundaries and
#' related locations are returned (Sampling Location or Equipment Location).
#' Default FALSE.
#' @return The output of the function is a `list` with slots:
#' \itemize{
#' \item \code{data} An `sf` object with the the boundary of the
#' site. If the boundary is missing from DEIMS-SDR, a `tibble` 
#' with the name and URI of the site.
#' \item \code{locations} A `tibble` with the locations' details 
#' if `with_locations` is TRUE, otherwise NULL.
#' } 
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @author  Micha Silver, phD (2021) \email{silverm@@post.bgu.ac.il}
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @importFrom dplyr mutate select as_tibble
#' @importFrom geojsonsf geojson_sf
#' @importFrom leaflet leaflet addTiles addPolygons addCircleMarkers
#' @importFrom sf st_sf st_sfc st_geometry_type
#' @importFrom utils capture.output URLencode
#' @keywords internal
#' @examples
#' \dontrun{
#' # LTER Zöbelboden
#' boundaries <- get_site_boundaries(
#'   deimsid = "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",
#'   show_map = TRUE,
#'   with_locations = TRUE
#' )
#' boundaries
#' }
#' @section The function output:
#' \figure{get_site_boundaries_fig.png}{Map of "LTER Zöbelboden, Austria" 
#' with locations}
#'
### function get_site_boundaries
get_site_boundaries <- function(
    deimsid,
    show_map = FALSE,
    with_locations = FALSE
) {
  res <- list(data = NULL, locations = NULL)
  
  # Extract only the UUID from the full DEIMS ID URL
  deimsid_uuid <- basename(deimsid)
  
  # --- Fetch boundaries from geoserver ---
  url_geoserver <- paste0(
    "https://deims.org/geoserver/deims/ows?",
    "service=WFS&version=2.0.0&request=GetFeature",
    "&TypeName=deims:deims_sites_boundaries",
    "&outputFormat=application%2Fjson",
    "&CQL_FILTER=deimsid='", deimsid_uuid, "'"  # UUID only, not the full URL
  )
  
  # Download raw GeoJSON — suppressWarnings avoids "incomplete final line"
  # triggered by readLines when the server does not terminate the response with \n
  geojson_raw <- tryCatch(
    suppressWarnings(paste(readLines(url_geoserver), collapse = "")),
    error = function(e) {
      warning("Failed to fetch WFS for: ", deimsid, "\n  -> ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(geojson_raw)) return(invisible(NULL))
  
  # Parse GeoJSON string into sf object
  geoBoundaries <- tryCatch(
    geojsonsf::geojson_sf(geojson_raw),
    error = function(e) {
      warning("Failed to parse GeoJSON for: ", deimsid, "\n  -> ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(geoBoundaries)) return(invisible(NULL))
  
  # --- Fallback if geometry is empty ---
  if (length(geoBoundaries$geometry) == 0) {
    qo <- queries_jq_deims[[get_deims_API_version()]]$site_info
    siteInfo_data <- .materialise_query(qo, deimsid, "site_info")
    
    if (is.null(siteInfo_data) || nrow(siteInfo_data) == 0L) {
      warning("No information found for DEIMS ID: ", deimsid)
      return(invisible(NULL))
    }
    
    # message("\n---- This site doesn't contain boundaries info. ----\n")
    res$data <- dplyr::select(siteInfo_data, "title", "uri")
    return(res)
  }
  
  # --- Process geometry ---
  geoBoundaries <- geoBoundaries |>
    dplyr::mutate(title = name, uri = deimsid, .before = geometry) |>
    dplyr::select(-c("name", "deimsid", "field_elevation_avg_value"))
  
  res$data <- geoBoundaries
  
  # --- Optional map ---
  map <- NULL
  if (show_map != FALSE) {
    map <- map_add_site(geoBoundaries)
  }
  
  # --- Optional locations ---
  if (with_locations) {
    locations <- sf::st_sf(sf::st_sfc(), crs = 4326)
    
    qo <- queries_jq_deims[[get_deims_API_version()]]$site_boundaries
    boundariesInfo <- .materialise_query(qo, deimsid, "site_boundaries")
    
    if (!is.null(boundariesInfo) && nrow(boundariesInfo) > 0L) {
      relatedLocations <- boundariesInfo$relatedLocations[[1]] |>
        dplyr::mutate(uri = paste0(id$prefix, id$suffix), .after = title) |>
        dplyr::select(-"id")
      
      if (nrow(relatedLocations) > 0L) {
        for (i in seq_len(nrow(relatedLocations))) {
          location <- get_location_info_internal(
            locationid = relatedLocations$uri[i],
            show_map = FALSE
          )
          if (show_map != FALSE) {
            map <- map_add_location(location$data, map)
          }
          locations <- rbind(locations, location$data)
        }
      }
    }
    
    # Notify if no locations were found for this site
    if (nrow(locations) == 0L) {
      message("\n---- This site doesn't contain any locations. ----\n")
    }
    
    res$locations <- locations
  }
  
  if (isTRUE(show_map)) print(map)
  
  res
}

#' Create location map
#' @description This is an internal function for adding a site geometry to
#' an exising map 
#' @param geoBoundaries A `sf` object. It is the location geodata.
#' @param map A `leaflet` object. If not NULL, the site is added on it. Default=NULL
#' @return `leaflet` object
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @author Paolo Tagliolato, phD (2020) \email{tagliolato.p@@irea.cnr.it}
#' @importFrom leaflet leaflet addTiles addPolygons
#' @keywords internal
#' @noRd
map_add_site <- function(geoBoundaries, map = NULL) {
  if (is.null(map)) {
    map <- leaflet::leaflet() |> leaflet::addTiles()
  }
  
  map |>
    leaflet::addPolygons(
      data        = geoBoundaries,
      color       = "white",
      weight      = 3,
      opacity     = 1,
      fill        = TRUE,
      fillColor   = "#eaa75e",
      fillOpacity = 0.8,
      popup       = paste0(
        "<b>Site title: </b>",
        "<br><a href='", geoBoundaries$uri,
        "' target='_blank'>", geoBoundaries$title,
        "</a><br>"
      )
    )
}
