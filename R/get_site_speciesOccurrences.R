#' @title eLTER get_site_speciesOccurrences function
#' @description This function acquires data from
#' GBIF \url{https://www.gbif.org} (via `rgbif`),
#' iNaturalist \url{https://www.inaturalist.org/} and
#' OBIS \url{https://obis.org/} and crops to an eLTER site
#' boundary, which is obtained from the DEIMS-SDR sites API.
#' @param deimsid A `character`. The DEIMS.iD of the site from
#' DEIMS-SDR website. DEIMS.iD information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @param list_DS A `character`. Data source to get data from, any
#' combination of gbif, inat and/or obis.
#' @param show_map A `boolean`. If TRUE a Leaflet map with occurrences
#' is shown. Default FALSE.
#' @param limit A `numeric`. Number of records to return. This is passed
#' across all sources. Default: 500 for each source. BEWARE: if you have a
#' lot of species to query for (e.g., n = 10), that's 10 * 500 = 5000, which
#' can take a while to collect. So, when you first query, set the limit to
#' something smallish so that you can get a result quickly, then do more as
#' needed.
#' @param exclude_inat_from_gbif A `boolean`. If TRUE, when list_DS contains
#' both "gbif" and "inat", filter out gbif records originating
#' from iNaturalist (in order to avoid duplicates). Default TRUE.
#' @return The output of the function is a `list` of `sf` one for each of the
#' repositories specified in the list_DS parameter.
#' @author Alessandro Oggioni, PhD (2020) \email{oggioni.a@@irea.cnr.it}
#' @author Paolo Tagliolato, PhD (2020) \email{tagliolato.p@@irea.cnr.it}
#' @author Martina Zilioli \email{zilioli.m@@irea.cnr.it}
#' @importFrom leaflet layersControlOptions addLayersControl addLegend
#' @importFrom leaflet addCircleMarkers addTiles addProviderTiles leaflet
#' @importFrom leaflet colorFactor
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tibble as_tibble
#' @importFrom dplyr select mutate filter
#' @importFrom spocc occ2df obis_search occ
#' @importFrom sf st_as_text st_as_sfc st_bbox st_as_sf
#' @export
#' @examples
#' \dontrun{
#' # terrestrial site Saldur River Catchment
#' occ_SRC <- get_site_speciesOccurrences(
#'   deimsid =
#'   "https://deims.org/97ff6180-e5d1-45f2-a559-8a7872eb26b1",
#'   list_DS = c("gbif", "inat"),
#'   show_map = FALSE,
#'   limit = 100
#' )
#' occ_SRC
#'
#' # marine site Gulf of Venice only obis
#' occ_GoV <- get_site_speciesOccurrences(
#'   deimsid =
#'   "https://deims.org/758087d7-231f-4f07-bd7e-6922e0c283fd",
#'   list_DS = "obis",
#'   show_map = TRUE,
#'   limit = 10
#' )
#' occ_GoV
#'
#' # marine site Gulf of Venice, all repositories are invoked
#' # gbif, inat and obis
#' occ_GoV_all <- get_site_speciesOccurrences(
#'   deimsid =
#'   "https://deims.org/758087d7-231f-4f07-bd7e-6922e0c283fd",
#'   list_DS = c("gbif", "inat", "obis"),
#'   show_map = TRUE,
#'   limit = 100
#' )
#' occ_GoV_all
#' }
#'
### function get_site_speciesOccurrences
get_site_speciesOccurrences <- function(
  deimsid,
  list_DS,
  show_map = FALSE,
  limit = 500,
  exclude_inat_from_gbif = TRUE
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
  site_occ_spocc <- NULL
  site_occ_spocc_obis <- NULL
  if (any(c("gbif", "inat") %in% list_DS)) {
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

  # find 0 record to dataset
  list_DS_exclude <- NULL
  if (!is.null(site_occ_spocc) && site_occ_spocc$gbif$meta$returned == 0) {
    list_DS_exclude <- c(list_DS_exclude, "gbif")
  }
  if (!is.null(site_occ_spocc) && site_occ_spocc$inat$meta$returned == 0) {
    list_DS_exclude <- c(list_DS_exclude, "inat")
  }
  if (!is.null(site_occ_spocc_obis) && nrow(site_occ_spocc_obis$results) == 0) {
    list_DS_exclude <- c(list_DS_exclude, "obis")
  }
  list_DS <- list_DS[!(list_DS %in% list_DS_exclude)]

  # combine results from occ calls to a single data.frame ----
  occ_df <- NULL
  if ("gbif" %in% list_DS) {
    occ_df_gbif <- site_occ_spocc$gbif$data[[1]] %>%
      tibble::as_tibble()
    if ("inat" %in% list_DS && exclude_inat_from_gbif) {
      occ_df_gbif <- occ_df_gbif %>%
        dplyr::filter(institutionCode != "iNaturalist")
    }
    occ_df_gbif <- occ_df_gbif %>%
      dplyr::select(
        name,
        longitude,
        latitude,
        prov,
        date = eventDate,
        key
      ) %>%
      dplyr::mutate(
        date = as.character(date)
      )
  
    if (nrow(occ_df_gbif) > 0) {
      occ_df <- rbind(occ_df, occ_df_gbif)
    } else {
      list_DS <- list_DS[!(list_DS == "gbif")]
    }
  }

  if ("inat" %in% list_DS) {
    occ_df_inat <- site_occ_spocc$inat$data[[1]] %>%
      tibble::as_tibble() %>%
      dplyr::select(
        name,
        longitude,
        latitude,
        prov,
        date = observed_on,
        key = id
      )
    occ_df <- rbind(occ_df, occ_df_inat)
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
    occ_df <- rbind(occ_df, occ_df_obis)
  }

  # print map ----
  if (length(list_DS) < 3) {
    my_palette <- RColorBrewer::brewer.pal(
      3,
      "Set1"
    )
    my_palette <- my_palette[seq(from = 1, to = length(list_DS))]
  } else {
    my_palette <- RColorBrewer::brewer.pal(
      length(list_DS),
      "Set1"
    )
  }
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
      occ_list$gbif <- occ_list$gbif %>%
        dplyr::filter(
          institutionCode != "iNaturalist"
        )
      if (nrow(occ_list$gbif) != 0) {
        occ_list$gbif <- sf::st_as_sf(
          occ_list$gbif,
          coords = c("longitude", "latitude"),
          crs = 4326
        )
      } else {
        occ_list$gbif <- NULL
      }
    }
    if ("inat" %in% list_DS) {
      occ_list$inat <- site_occ_spocc$inat$data[[1]]
      occ_list$inat <- sf::st_as_sf(
        occ_list$inat,
        coords = c("longitude", "latitude"),
        crs = 4326
      )
    }
    if ("obis" %in% list_DS) {
      occ_list$obis <- site_occ_spocc_obis$results
      occ_list$obis <- sf::st_as_sf(
        occ_list$obis,
        coords = c("decimalLongitude", "decimalLatitude"),
        crs = 4326
      )
    }
    return(occ_list)
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
      occ_list$gbif <- occ_list$gbif %>%
        dplyr::filter(
          institutionCode != "iNaturalist"
        )
      occ_list$gbif <- sf::st_as_sf(
        occ_list$gbif,
        coords = c("longitude", "latitude"),
        crs = 4326
      )
    }
    if ("inat" %in% list_DS) {
      occ_list$inat <- site_occ_spocc$inat$data[[1]]
      occ_list$inat <- sf::st_as_sf(
        occ_list$inat,
        coords = c("longitude", "latitude"),
        crs = 4326
      )
    }
    if ("obis" %in% list_DS) {
      occ_list$obis <- site_occ_spocc_obis$results
      occ_list$obis <- sf::st_as_sf(
        occ_list$obis,
        coords = c("decimalLongitude", "decimalLatitude"),
        crs = 4326
      )
    }
    return(occ_list)
  }
}
