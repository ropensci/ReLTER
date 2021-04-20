#' @title eLTER getSiteBoundaries function
#' @description This function allows to obtain a gpkg, a shp and a png versions (saved in the actual workdir) of the eLTER site boundaries, a view of the site boundaries on a leaflet map is also showed. All the info are taken from the DEIMS-SDR sites API. If the boundaries is missing an worning message will be proposed in the R console.
#' @param deimsid A `character`. It is the DEIMS iD of network make from DEIMS-SDR website. More information about DEIMS iD in this \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `gpkg`, a `shapefile` (shp) and a `html map` with boundaries of the site.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import jsonlite httr tibble sf leaflet mapview
#' @export
#' @examples
#' tSiteBoundaries <- getSiteBoundaries(deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe')
#' tSiteBoundaries
#'
### function getSiteBoundaries
# TODO: verify if is valid for multiple boundaries
getSiteBoundaries <- function(deimsid) {
  biomeColor <- tibble::tribble(
    ~geoBonBiome, ~fill, ~border,
    "marine", "#055ca8", "#057ae1",
    "coastal", "#43903f", "#5ecc58",
    "fresh_water_lakes", "#03a3b8", "#04d0eb",
    "fresh_water_rivers", "#03a3b8", "#04d0eb",
    "terrestrial", "#b07c03", "#e8a303"
  )
  geoBonBiome <- jsonlite::fromJSON(paste0("https://deims.org/", "api/sites/", substring(deimsid, 19)))$attributes$environmentalCharacteristics$geoBonBiome
  color <- biomeColor$fill[biomeColor$geoBonBiome == geoBonBiome]
  colorBorder <- biomeColor$border[biomeColor$geoBonBiome == geoBonBiome]
  
  q = '{title: .title, uri: "\\(.id.prefix)\\(.id.suffix)", boundaries: .attributes.geographic.boundaries}'
  url <- paste0("https://deims.org/", "api/sites/", substring(deimsid, 19))
  export <- httr::GET(url = url)
  jj <- suppressMessages(httr::content(export, "text"))
  invisible(capture.output(boundaries <- tibble::as_tibble(ReLTER::do_Q(q, jj))))
  if (!is.null(boundaries)) {
    geoBoundaries <- sf::st_as_sf(boundaries, wkt = "boundaries", crs = 4326)
    sf::write_sf(geoBoundaries, paste0("sites_", gsub(' ', '_', boundaries$title), ".gpkg"), append=FALSE)
    sf::st_write(geoBoundaries, paste0("sites_", gsub(' ', '_', boundaries$title), ".shp"), append=FALSE)
    map <- leaflet::leaflet(geoBoundaries) %>%
      leaflet::addTiles() %>%
      leaflet::addPolygons(color = color)
    mapview::mapshot(map, file = paste0("sites_", gsub(' ', '_', boundaries$title), ".png"), append=FALSE)
    map
  } else {
    warning('This site has not boundaries yet uploaded in DEIMS-SDR')
  }
}
