#' @title eLTER get_site_speciesOcc function
#' @description This function acquires data from
#' GBIF \url{https://www.gbif.org} (via `rgbif`),
#' iNaturalist \url{https://www.inaturalist.org/} and
#' OBIS \url{https://obis.org/} and crops to an eLTER site
#' boundary, which is obtained from the DEIMS-SDR sites API.
#' @param deimsid  a `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. More information about DEIMS ID from:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @param list_DS a `character`. Data source to get data from, any
#' combination of gbif, inat and/or obis.
#' @param show_map a `boolean`. If TRUE a Leaflet map with occurences
#' is shown. Default FALSE.
#' @param limit a `numeric`. Number of records to return. This is passed
#' across all sources. To specify different limits for each source, use
#' the options for each source (gbifopts, bisonopts, inatopts, and ebirdopts).
#' See Details for more. Default: 500 for each source. BEWARE: if you have a
#' lot of species to query for (e.g., n = 10), that's 10 * 500 = 5000, which
#' can take a while to collect. So, when you first query, set the limit to
#' something smallish so that you can get a result quickly, then do more as
#' needed.
#' @return The output of the function is a tibble with the list of occurrences
#' retived from species data sources selected into parameter list_DS.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom leaflet layersControlOptions addLayersControl addLegend
#' @importFrom leaflet addCircleMarkers addTiles addProviderTiles leaflet
#' @importFrom leaflet colorFactor
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tibble as_tibble
#' @importFrom dplyr select mutate
#' @importFrom spocc occ2df obis_search occ
#' @importFrom sf st_as_text st_as_sfc st_bbox
#' @export
#' @examples
#' \dontrun{
#' # terrestrial site Saldur River Catchment
#' occ_SRC <- get_site_speciesOcc(
#'   deimsid =
#'   "https://deims.org/97ff6180-e5d1-45f2-a559-8a7872eb26b1",
#'   list_DS = c("gbif", "inat"),
#'   show_map = TRUE,
#'   limit = 100
#' )
#' occ_SRC
#'
#' # marine site Gulf of Venice only obis
#' occ_GoV <- get_site_speciesOcc(
#'   deimsid =
#'   "https://deims.org/758087d7-231f-4f07-bd7e-6922e0c283fd",
#'   list_DS = c("obis"),
#'   show_map = TRUE,
#'   limit = 100
#' )
#' occ_GoV
#'
#' # marine site Gulf of Venice only gbif, inat and obis
#' occ_GoV_all <- get_site_speciesOcc(
#'   deimsid =
#'   "https://deims.org/758087d7-231f-4f07-bd7e-6922e0c283fd",
#'   list_DS = c("gbif", "inat", "obis"),
#'   show_map = TRUE,
#'   limit = 300
#' )
#' occ_GoV_all
#' }
#'
### function get_site_speciesOcc
get_site_speciesOcc <- function(
  deimsid,
  list_DS,
  show_map = FALSE,
  limit = 500
) {
  # First check that site has a boundary ----
  boundary <- ReLTER::get_site_info(
    deimsid,
    category = "Boundaries"
  )
  if (is.null(boundary) || !inherits(boundary, "sf")) {
    print("No boundary for requested DEIMS site.")
    return(NULL)
  } else {
    bbox_wkt <- sf::st_as_text(
      sf::st_as_sfc(
        sf::st_bbox(
          boundary
        )
      )
    )
  }

  # download occurrence by SPOCC by provide data sources ----
  if (c("gbif", "inat") %in% list_DS) {
    site_occ_spocc <- spocc::occ(
      from = list_DS,
      geometry = bbox_wkt,
      limit = limit,
      has_coords = TRUE
    )
  }
  if ("obis" %in% list_DS) {
    site_occ_spocc_obis <- spocc::obis_search(
      size = limit,
      geometry = bbox_wkt
    )
  }

  # combine results from occ calls to a single data.frame ----
  if (c("gbif", "inat") %in% list_DS) {
    occ_df <- spocc::occ2df(site_occ_spocc)
    occ_df <- occ_df %>%
      dplyr::mutate(
        date = as.character(occ_df$date)
      )
  }
  if ("obis" %in% list_DS) {
    occ_df_obis <- site_occ_spocc_obis$results %>%
      dplyr::mutate(
        prov = rep("obis", nrow(site_occ_spocc_obis$results))
      ) %>%
      dplyr::select(
        scientificName,
        decimalLongitude,
        decimalLatitude,
        prov,
        eventDate,
        id
      ) %>%
      tibble::as_tibble()
    names(occ_df_obis) <- c(
      "name", "longitude", "latitude",
      "prov", "date", "key"
    )
  }
  if (exists("occ_df") & exists("occ_df_obis")) {
    occ_df <- rbind(occ_df, occ_df_obis)
  } else if (exists("occ_df") & !exists("occ_df_obis")) {
    occ_df <- occ_df
  } else if (!exists("occ_df") & exists("occ_df_obis")) {
    occ_df <- occ_df_obis
  }

  # print map ----
  my_palette <- RColorBrewer::brewer.pal(
    length(list_DS),
    "Set1"
  )
  factpal <- leaflet::colorFactor(
    my_palette,
    levels = list_DS
  )
  if (show_map == TRUE) {
    occ_map <- leaflet::leaflet(occ_df) %>%
      leaflet::addProviderTiles(
        provider = "CartoDB.PositronNoLabels",
        group = "Basemap",
        layerId = 123
      ) %>%
      leaflet::addTiles(
        "http://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}.png"
      )

    groups <- unique(occ_df$prov)
    for (i in groups) {
      data <- occ_df[occ_df$prov == i, ]
      occ_map <- occ_map %>%
        leaflet::addCircleMarkers(
          data = data,
          lat = ~as.double(latitude),
          lng = ~as.double(longitude),
          popup = ~name,
          radius = 3,
          weight = 2,
          opacity = 0.5,
          fill = TRUE,
          fillOpacity = 0.2,
          color = ~factpal(prov),
          group = i
        )
    }
    occ_map <- occ_map %>%
      leaflet::addLegend(
        position = "bottomright",
        pal = factpal,
        values = ~list_DS,
        opacity = 1,
        title = "SPOCC Occurrence"
      ) %>%
      leaflet::addLayersControl(
        overlayGroups = groups,
        options = leaflet::layersControlOptions(
          collapsed = FALSE
        )
      )

    print(occ_map)
    # create tibble ----
    occ_list <- vector(
      mode = "list",
      length = length(
        unique(occ_df$prov)
      )
    )
    names(occ_list) <- unique(occ_df$prov)
    if ("gbif" %in% list_DS) {
      occ_list$gbif <- site_occ_spocc$gbif$data[[1]]
    }
    if ("inat" %in% list_DS) {
      occ_list$inat <- site_occ_spocc$inat$data[[1]]
    }
    if ("obis" %in% list_DS) {
      occ_list$obis <- site_occ_spocc_obis$results
    }

    occ_list
  } else {
    occ_map <- NULL
    # create tibble ----
    occ_list <- vector(
      mode = "list",
      length = length(
        unique(occ_df$prov)
      )
    )
    names(occ_list) <- unique(occ_df$prov)
    if ("gbif" %in% list_DS) {
      occ_list$gbif <- site_occ_spocc$gbif$data[[1]]
    }
    if ("inat" %in% list_DS) {
      occ_list$inat <- site_occ_spocc$inat$data[[1]]
    }
    if ("obis" %in% list_DS) {
      occ_list$obis <- site_occ_spocc_obis$results
    }
    return(occ_list)
  }
}
