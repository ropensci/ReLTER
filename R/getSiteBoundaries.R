#' @title eLTER_getSiteBoundaries
#' @description This function allows to obtain a gpkg and a shp version (saved in the actual workdir) of the eLTER site boundaries, a view of the site boundaries on a leaflet map is also showed. All the info are taken from the DEIMS-SDR sites API. If the boundaries is missing an worning message will be proposed in the R console.
#' @param deimsid
#' @return The output of the function is a xxx with boundaries of the site.
#' @author Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
#' @import jsonlite httr tibble sf leaflet
#' @export
#' @examples
#' getSiteBoundaries(deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe')
#'
### function getSiteBoundaries
getSiteBoundaries <- function(deimsid) {
  q = '{title: .title, uri: "\\(.id.prefix)\\(.id.suffix)", boundaries: .attributes.geographic.boundaries}'
  url <- paste0("https://deims.org/", "api/sites/", substring(deimsid, 19))
  export <- httr::GET(url = url)
  jj <- httr::content(export, "text")
  boundaries <- tibble::as_tibble(do_Q(q, jj))
  
  if (!is.null(boundaries)) {
    geoBoundaries <- sf::st_as_sf(boundaries, wkt = "boundaries", crs = 4326)
    sf::write_sf(geoBoundaries, paste0("sites_", gsub(' ', '_', boundaries$title), ".gpkg"), append=FALSE)
    sf::st_write(geoBoundaries, paste0("sites_", gsub(' ', '_', boundaries$title), ".shp"), append=FALSE)
    leaflet::leaflet(geoBoundaries) %>%
      leaflet::addTiles() %>%
      leaflet::addPolygons(color = "red")
  } else {
    warning('This site has not boundaries yet uploaded in DEIMS-SDR')
  }
}
