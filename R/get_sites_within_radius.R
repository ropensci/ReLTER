#' Get all sites within a given distance of a point location
#' @description `r lifecycle::badge("experimental")`
#' retrieve all sites within a given distance from 
#' the center point (in coordinates lat, lon), and in 
#' the (optionally) specified elevation range.
#' @param lat latitude of radius center
#' @param lon longitude of radius center
#' @param distance distance from point (actually it is in degrees for a limitation of the endpoint)
#' @param elevation_range min and max elevation in meters
#' @param show_map `logical` print a map with retrieved sites (default is TRUE)
#' @return list of deims_id, distance from given coordinates
#' @importFrom leaflet leaflet addCircles addTiles
#' @seealso [geojsonsf::geojson_sf()]
#' @examples
#' # example code
#' get_sites_within_radius(lat=39.1386, lon=-8.33305, distance=900)
#' @export
#' 
get_sites_within_radius <- function(lat=39.1386, lon=-8.33305, 
                                    distance=1, 
                                    elevation_range=c(NULL, NULL),
                                    show_map=TRUE){
  
  deps <- c("geojsonsf")
  deps_missing <- !sapply(deps, requireNamespace, quietly = TRUE)
  
  if (sum(deps_missing) > 0) {
    stop("You need to install the following Suggested packages to use this function.
           Please install them with:
           install.packages(c(\"geojsonsf\"))")
  } else {
    geojson_sf_fx <- getNamespace("geojsonsf")$geojson_sf
  }
  
  #capa="https://deims.org/geoserver/deims/ows?service=WFS&version=2.0.0&request=GetCapabilities&outputFormat=application%2Fjson"
  url.geoserver<-paste0("https://deims.org/geoserver/deims/ows?",
                        "service=WFS&version=2.0.0&",
                        "request=GetFeature&",
                        "TypeName=%s&",
                        "outputFormat=application/json&",
                        "CQL_FILTER=%s"
                        )
  
  # filter=paste0('(<fes:Filter>',
  #               '<fes:DWithin>',
  #                   '<fes:ValueReference>gml:boundedBy</fes:ValueReference>',
  #                   '<gml:Point srsName="http://www.opengis.net/def/crs/epsg/0/4326">',
  #                   '<gml:pos>%s %s</gml:pos>',
  #                   '</gml:Point>',
  #                   '<fes:Distance uom="m">%s</fes:Distance>',
  #                   '</fes:DWithin>',
  #                   '</fes:Filter>)')
  # 
  CQL_FILTER="DWITHIN(geom,Point(%s %s),%s,kilometers)" 
  # NOTE: ISSUE IN GEOSERVER: Even if it requires "kilometers", 
  # "degrees" are used. 
  # See https://stackoverflow.com/questions/45333651/geoserver-dwithin-not-filtering-points-correctly
  filt=sprintf(CQL_FILTER,lat, lon, distance)
  
  filt=if(is.null(elevation_range)) filt else 
    paste0(filt, " AND ", sprintf("field_elevation_avg_value BETWEEN %s AND %s", elevation_range[1], elevation_range[2])
           )
  
           
  typeNames<-list(
    deims_all_sites="deims:deims_all_sites",
    boundaries="deims:deims_sites_boundaries&",
    GERI_sites="deims:geri_sites",
    ILTER_sites="deims:ilter_all_formal",
    LTER_Africa_sites="deims:lter_africa_formal",
    LTER_Americas_sites="deims:lter_americas_formal",
    LTER_East_Asia_Pacific_sites="deims:lter_eap_formal",
    LTER_Europe_sites="deims:lter_eu_formal")
  
  qurl<-sprintf(url.geoserver, typeNames$deims_all_sites, URLencode(filt))
  #cat(qurl)  
  if(TRUE){
  #               
  # sprintf()
  features<-geojson_sf_fx(qurl)
  if (show_map == TRUE) {
    map <- leaflet::leaflet(features) %>%
      leaflet::addTiles() %>%
      leaflet::addCircles(popup = paste0("<a href='",features$deimsid,"'>",features$deimsid,"</a>"))
      
    print(map)
  }
  features
  }
}

#' select sites within a 3d bounding box
#' @description `r lifecycle::badge("experimental")`
#' select sites within a 3d bounding box
#' @param bbox bounding box
#' @param elevation_range elevation range
#' @param show_map show the map
#' @importFrom leaflet leaflet addCircles addTiles
#' @seealso [geojsonsf::geojson_sf()]
#' @examples
#' bbx3d<-list(bbx=data.frame(x=c(6.718290, 9.805761), 
#'                 y=c(44.79938, 47.11089)), 
#'             elevation_range=c(1200, 9000))
#' get_sites_within_3d_bounding_box(bbx3d)
#'   
#' @export
get_sites_within_3d_bounding_box<-function(bbox, elevation_range=NULL, show_map=TRUE){
  
  # deps <- c("geojsonsf")
  # deps_missing <- !sapply(deps, requireNamespace, quietly = TRUE)
  # 
  # if (sum(deps_missing) > 0) {
  #   stop("You need to install the following Suggested packages to use this function.
  #          Please install them with:
  #          install.packages(c(\"leaflet\", \"geojsonsf\"))")
  # } else {
  #   requireNamespace("leaflet")
  #   requireNamespace("geojsonsf")
  # }
  # 
  # if(class(bbox)=="list" && "bbx" %in% names(bbox)){
  #   bbx <- bbox$bbx
  #   if("elevation_range" %in% names(bbox)){
  #     elevation_range <- bbox$elevation_range
  #   }
  # }
  deps <- c("geojsonsf")
  deps_missing <- !sapply(deps, requireNamespace, quietly = TRUE)
  
  if (sum(deps_missing) > 0) {
    stop("You need to install the following Suggested packages to use this function.
           Please install them with:
           install.packages(c(\"geojsonsf\"))")
  } else {
    geojson_sf_fx <- getNamespace("geojsonsf")$geojson_sf
  }
  
  url.geoserver<-paste0("https://deims.org/geoserver/deims/ows?",
                        "service=WFS&version=2.0.0&",
                        "request=GetFeature&",
                        "TypeName=%s&",
                        "outputFormat=application/json&",
                        "CQL_FILTER=%s"
  )
  
  #BBOX_FILTER="bbox=%s,%s,%s,%s"
  CQL_FILTER="BBOX(geom,%s,%s,%s,%s)"
  filt=sprintf(CQL_FILTER,bbx[1,2], bbx[1,1], bbx[2,2], bbx[2,1])
  
  filt=if(is.null(elevation_range)) filt else 
    paste0(filt," AND ", 
           sprintf("field_elevation_avg_value BETWEEN %s AND %s", 
                   elevation_range[1], elevation_range[2]))
  
  
  typeNames<-list(
    deims_all_sites="deims:deims_all_sites",
    boundaries="deims:deims_sites_boundaries&",
    GERI_sites="deims:geri_sites",
    ILTER_sites="deims:ilter_all_formal",
    LTER_Africa_sites="deims:lter_africa_formal",
    LTER_Americas_sites="deims:lter_americas_formal",
    LTER_East_Asia_Pacific_sites="deims:lter_eap_formal",
    LTER_Europe_sites="deims:lter_eu_formal")
  
  qurl<-sprintf(url.geoserver, typeNames$deims_all_sites, URLencode(filt))
  #cat(qurl)  
  if(TRUE){
    #               
    # sprintf()
    features<-geojson_sf_fx(qurl)#geojsonsf::geojson_sf(qurl)
    if (show_map == TRUE) {
      map <- leaflet::leaflet(features) %>%
        leaflet::addTiles() %>%
        leaflet::addCircles(popup = paste0("<a href='",features$deimsid,"'>",features$deimsid,"</a>"))
      
      print(map)
    }
    features
  }
}
# get_sites_within_3d_bounding_box(bbx)
# 
# get_sites_within_3d_bounding_box(define_bbx(), elevation_range = c(100, 2000))
# get_sites_within_radius(elevation_range = c(0,2000))
