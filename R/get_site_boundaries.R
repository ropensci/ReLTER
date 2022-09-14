#' @title eLTER get_site_boundaries function
#' @description This function retrieves the boundary of a specified eLTER site
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
#' @importFrom httr RETRY content
#' @importFrom utils capture.output
#' @importFrom sf st_as_sf write_sf st_write
#' @importFrom leaflet leaflet addTiles addPolygons
#' @keywords internal
#'
### function get_site_boundaries
get_site_boundaries <- function(deimsid, show_map = FALSE) {
  q <- '{title: .title,
        uri: "\\(.id.prefix)\\(.id.suffix)",
        boundaries: .attributes.geographic.boundaries
       }'
  jj <- get_id(deimsid, "sites")
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        boundaries <- dplyr::as_tibble(do_Q(q, jj))
      )
    )
    if (!is.null(boundaries)) {
      if (is.na(boundaries$boundaries)) {
        message(
          "\n----\nThis site does not have boundaries uploaded to DEIMS-SDR.\n",
          "Please verify in the site page: ",
          deimsid,
          "\n----\n"
        )
        geoBoundaries <- boundaries
      } else {
        geoBoundaries <- sf::st_as_sf(boundaries,
                                      wkt = "boundaries",
                                      crs = 4326
                                      )
        if (show_map == TRUE) {
          map <- leaflet::leaflet(geoBoundaries) %>%
            leaflet::addTiles() %>%
            leaflet::addPolygons()
          print(map)
        }
        #return(geoBoundaries)
      }
    # } else {
    #   warning(
    #     "\n ----This site does not have boundaries uploaded to DEIMS-SDR.",
    #     "Please verify in the site page (",
    #     deimsid,
    #     ")---- \n"
    #   )
    #   geoBoundaries <- NULL
    }
  } else {
    message("\n----\nThe requested page could not be found.",
    "Please check the DEIMS ID\n----\n")
    geoBoundaries <- NULL
  }
  geoBoundaries
}
