#' eLTER get_site_boundaries function
#' @description `r lifecycle::badge("stable")`
#' This internal function retrieves the boundary of a specified eLTER site
#' and a view of the site boundaries on a leaflet map is shown.
#' All the info are taken from the DEIMS-SDR sites API.
#' If the boundary is missing, a warning message is printed in the R console.
#' @param deimsid A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @param show_map A `boolean`. When TRUE the boundary will be plotted on a
#' Leaflet map. Default FALSE.
#' @param with_locations A `boolean`. When TRUE all site boundaries and
#' related locations are returned (Sampling Location or Equipment Location).
#' Default FALSE.
#' @return The output of the function is an `sf` object, the boundary of the
#' site or NA if the boundary is missing from DEIMS-SDR. In addition, as
#' `html map` with boundaries of the site is plotted.
#' If the parameter `with_locations` is TRUE return a list woth the boundary
#' and all related locations declared in DEIMS-SDR. A map with these is printed
#' if `show_map` is TRUE.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @author  Micha Silver, phD (2021) \email{silverm@@post.bgu.ac.il}
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @importFrom dplyr mutate select as_tibble
#' @importFrom geojsonsf geojson_sf
#' @importFrom leaflet leaflet addTiles addPolygons
#' @importFrom leaflet addCircleMarkers
#' @importFrom sf st_sf st_sfc st_geometry_type
#' @importFrom utils capture.output
#' @export
#' @examples
#' # LTER Zöbelboden
#' boundaries <- get_site_boundaries(
#'   deimsid = "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",
#'   show_map = TRUE,
#'   with_locations = TRUE
#' )
#' boundaries
#' 
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
  res<-list(
    data=NULL,
    map=NULL,
    locations=NULL
  )
  url.geoserver <- paste0("https://deims.org/geoserver/deims/ows?",
                          "service=WFS&version=2.0.0&request=GetFeature&TypeName=deims:deims_sites_boundaries&",
                          "outputFormat=application%2Fjson&CQL_FILTER=deimsid='", URLencode(deimsid), "'"
  )
  geoBoundaries <- geojsonsf::geojson_sf(url.geoserver)
  siteInfo <- get_site_info(deimsid = deimsid)
  
  # programma di refactoring:
  # calcolo in ogni caso site boundaries e lo metto in res$data
  # calcolo se richieste tutte le locations e le metto in res$locations
  # calcolo se richiesta la mappa. La base di partenza è map=mappa del sito
  #  a questa se le location non sono vuote le aggiungo.
  #
  # mi servono funzioni per calcolare le mappe:
  #  1. funzione per creare leaflet (di base) con sito
  #  2. funzione per AGGIUNGER leaflet di una location (da usare incrementalmente)
  # domanda: è possibile usare la funzione get_location_info(...,show_map=TRUE)? il valore restituito è
  # una lista list(data=location, map=mappa)
  # Sarebbe interessante fare tutto con quella funzione ma per non modificare la firma si potrebbe:
  # creare fx in file get_location_info in cui spostare il codice che da una location crea il suo leaflet.
  # questa fx avrebbe un ulteriore argomento "map": se non è nullo usa quella come mappa di base (map<-map %>% leaflet::addTile(...))
  # QUI allora userei get_location_info(show_map=FALSE) in modo da ottenere SOLO le location da slot "data".
  #     poi userei la funzione che crea il leflet passando via via la mappa che cresce.
  
  if(length(geoBoundaries$geometry) == 0 && !is.null(siteInfo)){
    geoBoundaries <- siteInfo %>% dplyr::select("title", "uri")
    message("\n---- This site doesn't contain geo info. ----\n")
    res$data = geoBoundaries
    return(res)
  }
  
  geoBoundaries <- geoBoundaries %>%
    dplyr::mutate(title = name, uri = deimsid, .before = geometry) %>%
    dplyr::select(-c("name", "deimsid", "field_elevation_avg_value"))
  
  res$data=geoBoundaries
  
  map<-NULL
  if(show_map){
    map <- map_add_site(geoBoundaries)
  }
  
  if(with_locations){
    boundaries <- list(
      site = "",
      locations = sf::st_sf(
        sf::st_sfc(),
        crs = 4326
      )
    )

    boundaries$site <- geoBoundaries 
    qo <- queries_jq_deims[[get_deims_API_version()]]$site_boundaries 
    jj <- get_id(deimsid, qo$path)
    if (is.na(attr(jj, "status"))) {
      invisible(
        utils::capture.output(
          boundariesInfo <- dplyr::as_tibble(do_Q(qo$query, jj))
        )
      )
    }
    relatedLocations <- boundariesInfo$relatedLocations[[1]] %>%
      dplyr::mutate(uri = paste0(
        id$prefix, id$suffix
      ), .after = title) %>%
      dplyr::select(-c(
        "id"
      ))
    
    for (i in seq_len(nrow(relatedLocations))) {
      location <- get_location_info(
        locationid = relatedLocations$uri[i], show_map = FALSE
      )
      if(show_map == TRUE){
        map<-map_add_location(location, map)
      }
      
      boundaries$locations <- boundaries$locations %>% rbind(location)
    }
    
    res$locations<-boundaries$locations
  }
  if (show_map == TRUE) {
    res$map<-map
    print(map)
  }
  
  return(res)
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
map_add_site <- function(geoBoundaries, map=NULL){
  #map <- leaflet::leaflet(geoBoundaries) %>%
  if(is.null(map)){
    map<-leaflet::leaflet() %>%
      leaflet::addTiles()
  }
  map <-  map %>%
    leaflet::addPolygons(
      data = geoBoundaries,
      color = "white",
      weight = 3,
      opacity = 1,
      fill = TRUE,
      fillColor = "#eaa75e",
      fillOpacity = 0.8,
      popup = paste0(
        "<b>Site title: </b>",
        "<br><a href='",
        geoBoundaries$uri,
        "' target='_blank'>",
        geoBoundaries$title,
        "</a><br>"
      )
    )
  return(map)
}

