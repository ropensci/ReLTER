#' @title eLTER getSiteBoundaries function
#' @description This function allows to obtain html map of the eLTER site
#' boundaries, a view of the site boundaries on a leaflet map is also
#' showed. All the info are taken from the DEIMS-SDR sites API. If the
#' boundaries is missing an warning message will be proposed in the R console.
#' @param deimsid A `character`. It is the DEIMS iD of network make from
#' DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a
#' `html map` with boundaries of the site.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom tibble tribble 
#' @importFrom dplyr as_tibble
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @importFrom utils capture.output
#' @importFrom sf st_as_sf write_sf st_write
#' @importFrom leaflet leaflet addTiles addPolygons
#' @importFrom mapview mapshot
#' @export
#' @examples
#' tSiteBoundaries <- getSiteBoundaries(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' )
#' tSiteBoundaries
#' 
#' eisenwurzen <- getSiteBoundaries(
#'   deimsid = "https://deims.org/d0a8da18-0881-4ebe-bccf-bc4cb4e25701"
#' )
#' eisenwurzen
#' 
### function getSiteBoundaries
getSiteBoundaries <- function(deimsid) {
  biomeColor <- tibble::tribble(
    ~geoBonBiome, ~fill, ~border,
    "marine", "#055ca8", "#057ae1",
    "coastal", "#43903f", "#5ecc58",
    "fresh_water_lakes", "#03a3b8", "#04d0eb",
    "fresh_water_rivers", "#03a3b8", "#04d0eb",
    "terrestrial", "#b07c03", "#e8a303"
  )
  geoBonBiome <- jsonlite::fromJSON(
    paste0(
      "https://deims.org/",
      "api/sites/",
      sub("^.+/", "", deimsid)
    )
  )$attributes$environmentalCharacteristics$geoBonBiome
  color <- biomeColor$fill[biomeColor$geoBonBiome == geoBonBiome]
  colorBorder <- biomeColor$border[biomeColor$geoBonBiome == geoBonBiome]
  q <- '{title: .title,
        uri: "\\(.id.prefix)\\(.id.suffix)",
        boundaries: .attributes.geographic.boundaries
       }'
  url <- paste0(
    "https://deims.org/",
    "api/sites/",
    sub("^.+/", "", deimsid)
  )
  export <- httr::GET(url = url)
  jj <- suppressMessages(httr::content(export, "text"))
  status <- jj %>% 
    jqr::jq(as.character('{status: .errors.status}')) %>% 
    textConnection() %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE) %>%
    dtplyr::lazy_dt() %>% 
    dplyr::as_tibble()
  if (is.na(status)) {
    invisible(
      utils::capture.output(
        boundaries <- dplyr::as_tibble(ReLTER:::do_Q(q, jj))
      )
    )
    if (!is.null(boundaries)) {
      if (is.na(boundaries$boundaries)) {
        warning(
          "\n ----This site has not boundaries yet uploaded in DEIMS-SDR.
      Please verify in the site page (",
          deimsid,
          ")---- \n"
        )
        geoBoundaries <- boundaries
        map <- NULL
      } else {
        geoBoundaries <- sf::st_as_sf(boundaries, wkt = "boundaries", crs = 4326)
        # TODO: add this part of the function when it will be possible to write files in the temp folder.
        # sf::write_sf(
        #   geoBoundaries,
        #   paste0("sites_", gsub(" ", "_", boundaries$title), ".gpkg"),
        #   append = FALSE
        # )
        # sf::st_write(
        #   geoBoundaries,
        #   paste0("sites_", gsub(" ", "_", boundaries$title), ".shp"),
        #   append = FALSE
        # )
        map <- leaflet::leaflet(geoBoundaries) %>%
          leaflet::addTiles() %>%
          leaflet::addPolygons(fillColor = color, color = colorBorder)
        # mapview::mapshot(
        #   map,
        #   file = paste0("sites_", gsub(" ", "_", boundaries$title), ".png"),
        #   append = FALSE
        # )
      }
    } else {
      warning(
        "\n ----This site has not boundaries yet uploaded in DEIMS-SDR.
      Please verify in the site page (",
        deimsid,
        ")---- \n"
      )
      geoBoundaries <- NULL
      map <- NULL
    }
  } else {
    message("\n---- The requested page could not be found. Please check again the DEIMS.iD ----\n")
    geoBoundaries <- NULL
    map <- NULL
  }
  print(map)
  geoBoundaries
}
