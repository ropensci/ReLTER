#' @title eLTER get_site_boundaries function
#' @description This function retrieves the boundary of a specified eLTER site
#' and a view of the site boundaries on a leaflet map is shown.
#' All the info are taken from the DEIMS-SDR sites API.
#' If the boundary is missing, a warning message is printed in the R console.
#' @param deimsid A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. More information about DEIMS ID in this page:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is an `sf` object, the boundary of the
#' site or NA if the boundary is missing from DEIMS-SDR. In addition, as
#' `html map` with boundaries of the site is plotted.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @author  Micha Silver, phD (2021) \email{silverm@@post.bgu.ac.il}
#' @importFrom tibble tribble
#' @importFrom dplyr as_tibble
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @importFrom utils capture.output
#' @importFrom sf st_as_sf write_sf st_write
#' @importFrom leaflet leaflet addTiles addPolygons
#' @importFrom mapview mapshot
#' @export
#' @keywords internal
#' @examples
#' tSiteBoundaries <- get_site_boundaries(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' )
#' tSiteBoundaries
#'
#' eisenwurzen <- get_site_boundaries(
#'   deimsid = "https://deims.org/d0a8da18-0881-4ebe-bccf-bc4cb4e25701"
#' )
#' eisenwurzen
#'
### function get_site_boundaries
get_site_boundaries <- function(deimsid) {
  # biomeColor <- tibble::tribble(
  #   ~geoBonBiome, ~fill, ~border,
  #   "Marine", "#055ca8", "#057ae1",
  #   "Coastal", "#43903f", "#5ecc58",
  #   "Fresh water lakes", "#03a3b8", "#04d0eb",
  #   "Fresh water rivers", "#03a3b8", "#04d0eb",
  #   "Terrestrial", "#b07c03", "#e8a303"
  # )
  # geoBonBiome <- jsonlite::fromJSON(
  #   paste0(
  #     "https://deims.org/",
  #     "api/sites/",
  #     sub("^.+/", "", deimsid)
  #   )
  # )$attributes$environmentalCharacteristics$geoBonBiome
  # color <- biomeColor$fill[biomeColor$geoBonBiome == geoBonBiome[1]]
  # colorBorder <- biomeColor$border[biomeColor$geoBonBiome == geoBonBiome[1]]
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
    jqr::jq(as.character("{status: .errors.status}")) %>%
    textConnection() %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE) %>%
    dtplyr::lazy_dt() %>%
    dplyr::as_tibble()
  if (is.na(status)) {
    invisible(
      utils::capture.output(
        boundaries <- dplyr::as_tibble(do_Q(q, jj))
      )
    )
    if (!is.null(boundaries)) {
      if (is.na(boundaries$boundaries)) {
        warning(
          "\n----\nThis site does not have boundaries uploaded to DEIMS-SDR.
Please verify in the site page (",
          deimsid,
          ")\n----\n"
        )
        geoBoundaries <- boundaries
        # map <- NULL
      } else {
        geoBoundaries <- sf::st_as_sf(
          boundaries,
          wkt = "boundaries",
          crs = 4326
        )
        # TODO: add this part of the function when it will be possible to write
        # files in the temp folder.
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

        ##---------------------------------------------------
        # MS: Temporarily disable printing of map, for testing
        # map <- leaflet::leaflet(geoBoundaries) %>%
        #   leaflet::addTiles() %>%
        #   leaflet::addPolygons(fillColor = color, color = colorBorder)
        # print(map)
        ##--------------------------------------------------
        # mapview::mapshot(
        #   map,
        #   file = paste0("sites_", gsub(" ", "_", boundaries$title), ".png"),
        #   append = FALSE
        # )
        geoBoundaries
      }
    } else {
      warning(
        "\n ----This site does not have boundaries uploaded to DEIMS-SDR.
      Please verify in the site page (",
        deimsid,
        ")---- \n"
      )
      geoBoundaries <- NULL
      # map <- NULL
    }
  } else {
    message("\n----\nThe requested page could not be found.
Please check the DEIMS ID\n----\n")
    geoBoundaries <- NULL
    # map <- NULL
  }
  geoBoundaries
}
