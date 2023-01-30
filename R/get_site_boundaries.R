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
#' @return The output of the function is an `sf` object, the boundary of the
#' site or NA if the boundary is missing from DEIMS-SDR. In addition, as
#' `html map` with boundaries of the site is plotted.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @author  Micha Silver, phD (2021) \email{silverm@@post.bgu.ac.il}
#' @importFrom tibble tribble
#' @importFrom dplyr as_tibble
#' @importFrom jsonlite fromJSON
#' @importFrom utils capture.output
#' @importFrom sf st_as_sf write_sf st_write
#' @importFrom leaflet leaflet addTiles addPolygons
#' @keywords internal
#'
### function get_site_boundaries
get_site_boundaries <- function(deimsid, show_map = FALSE) {
  
    # TODO; use WFS eg https://deims.org/geoserver/deims/ows?service=WFS&version=2.0.0&request=GetFeature&TypeName=deims:deims_sites_boundaries&outputFormat=application%2Fjson&CQL_FILTER=deimsid=%27https://deims.org/
    url.geoserver<-paste0("https://deims.org/geoserver/deims/ows?",
                        "service=WFS&version=2.0.0&request=GetFeature&TypeName=deims:deims_sites_boundaries&",
                          "outputFormat=application%2Fjson&CQL_FILTER=deimsid=%27",URLencode(deimsid), "%27")
    #cat(url.geoserver)
    
    # if(length(geoBoundaries$geometry==0)){
    #   message(
    #     "\n----\nThis site does not have boundaries uploaded to DEIMS-SDR.\n",
    #     "Please verify in the site page: ",
    #     deimsid,
    #     "\n----\n"
    #   )
    # } else {
    geoBoundaries<-geojsonsf::geojson_sf(url.geoserver) %>% dplyr::mutate(title=name, uri=deimsid, .keep="unused")
    
      if (show_map == TRUE) {
        map <- leaflet::leaflet(geoBoundaries) %>%
          leaflet::addTiles() %>%
          leaflet::addPolygons()
        print(map)
      }
    #}
    
  # else 
  # {
  #   qo <- queries_jq[[get_deims_API_version()]]$site_boundaries
  #   jj <- get_id(deimsid, qo$path)
  #   if (is.na(attr(jj, "status"))) {
  #     invisible(
  #       utils::capture.output(
  #         boundaries <- dplyr::as_tibble(do_Q(qo$query, jj))
  #       )
  #     )
  #     if (!is.null(boundaries)) {
  #       if (is.na(boundaries$boundaries)) {
  #         message(
  #           "\n----\nThis site does not have boundaries uploaded to DEIMS-SDR.\n",
  #           "Please verify in the site page: ",
  #           deimsid,
  #           "\n----\n"
  #         )
  #         geoBoundaries <- boundaries
  #       } else {
  #         geoBoundaries <- sf::st_as_sf(boundaries,
  #                                       wkt = "boundaries",
  #                                       crs = 4326
  #         )
  #         if (show_map == TRUE) {
  #           map <- leaflet::leaflet(geoBoundaries) %>%
  #             leaflet::addTiles() %>%
  #             leaflet::addPolygons()
  #           print(map)
  #         }
  #         #return(geoBoundaries)
  #       }
  #       # } else {
  #       #   warning(
  #       #     "\n ----This site does not have boundaries uploaded to DEIMS-SDR.",
  #       #     "Please verify in the site page (",
  #       #     deimsid,
  #       #     ")---- \n"
  #       #   )
  #       #   geoBoundaries <- NULL
  #     }
  #   } else {
  #     message("\n----\nThe requested page could not be found.",
  #             "Please check the DEIMS ID\n----\n")
  #     geoBoundaries <- NULL
  #   }
  # }
  geoBoundaries
}
#'
#' sitosenza<-get_site_boundaries(deimsid = "https://deims.org/a497328f-1e2d-416c-bc25-6ccf31965af4")
#' sitocon<-get_site_boundaries(deimsid = "https://deims.org/c0738b00-854c-418f-8d4f-69b03486e9fd", show_map = T)
#' 
