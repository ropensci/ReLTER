#' Retrieve species occurrences within an eLTER site boundary
#' @description `r lifecycle::badge("stable")`
#' This function downloads species occurrence records from
#' GBIF \url{https://www.gbif.org},
#' iNaturalist \url{https://www.inaturalist.org/} and
#' OBIS \url{https://obis.org/} and intersects them with the boundary of
#' an eLTER site retrieved from the DEIMS-SDR API
#' \url{https://deims.org/}. Only occurrences falling within the site
#' polygon are returned, enriched with eLTER site metadata.
#' @param deimsid A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @param list_DS A `character` vector. Data sources to query; any
#' combination of `"gbif"`, `"inat"`, and/or `"obis"`.
#' @param show_map A `boolean`. If `TRUE` the `leaflet` map is both printed
#' and returned in the output list as `$map`. If `FALSE` the map is not
#' printed but is still built and returned in `$map` for later use.
#' Default `FALSE`.
#' @param limit A `numeric`. Maximum number of records to download per
#' source. Default `500`. Note that when querying for many species the
#' total number of records can be large and slow to download; start with
#' a small value (e.g. `10`) to verify results before increasing.
#' @param exclude_inat_from_gbif A `boolean`. If `TRUE`, and both
#' `"gbif"` and `"inat"` are in `list_DS`, records originating from
#' iNaturalist are removed from the GBIF results to avoid duplicates.
#' Default `TRUE`.
#' @return A `list` with one `sf` element per data source (named `gbif`,
#' `inat`, `obis`) containing only occurrences that fall within the site
#' boundary, and a `map` element with a `leaflet` object. Each `sf`
#' element contains the occurrence geometry and the following eLTER site
#' metadata fields (prefixed with `eLTER_`): `title`, `uri`, `created`,
#' `changed`, `geoCoord`, `country`, `geoElev.avg`, `geoElev.min`,
#' `geoElev.max`, `biogeographicalRegion`, `biome`, `ecosystemType`,
#' `eunisHabitat`, `landforms`, `geoBonBiome`, `geology`, `hydrology`,
#' `soils`, `vegetation`, `size.value`.
#' If no occurrences are found within the boundary for a given source,
#' that source is omitted from the list and an informative message is
#' printed. Returns `invisible(NULL)` if the site has no boundary or if
#' no occurrences are found for any source.
#' @author Alessandro Oggioni, PhD (2020) \email{oggioni.a@@irea.cnr.it}
#' @author Paolo Tagliolato, PhD (2020) \email{tagliolato.p@@irea.cnr.it}
#' @author Martina Zilioli \email{zilioli.m@@irea.cnr.it}
#' @importFrom dplyr filter select rename_with all_of
#' @importFrom leaflet leaflet addTiles addProviderTiles addCircleMarkers
#' @importFrom leaflet addLegend addLayersControl layersControlOptions
#' @importFrom leaflet colorFactor
#' @importFrom lubridate as_datetime as_date
#' @importFrom sf st_as_text st_as_sfc st_bbox st_as_sf st_transform
#' @importFrom sf st_intersection st_coordinates
#' @seealso [spocc::occ()]
#' @seealso [spocc::obis_search()]
#' @seealso [RColorBrewer::brewer.pal()]
#' @seealso [get_site_info()]
#' @export
#' @examples
#' \dontrun{
#' # Terrestrial site: Saldur River Catchment (GBIF and iNaturalist, excluding records sourced from iNaturalist)
#' occ_SRC <- get_site_speciesOccurrences(
#'   deimsid = "https://deims.org/97ff6180-e5d1-45f2-a559-8a7872eb26b1",
#'   list_DS = c("gbif", "inat"),
#'   show_map = TRUE,
#'   limit = 50,
#'   exclude_inat_from_gbif = TRUE
#' )
#' occ_SRC
#' 
#' # Terrestrial site: Gran Paradiso National Park (only GBIF considering, excluding records sourced from iNaturalist)
#' occ_GPNP <- get_site_speciesOccurrences(
#'   deimsid = "https://deims.org/15c3e841-8494-42d2-a44e-c49a0ff25946",
#'   list_DS = "gbif",
#'   show_map = TRUE,
#'   limit = 50,
#'   exclude_inat_from_gbif = TRUE
#' )
#' occ_GPNP
#'
#' # Marine site: Gulf of Venice (OBIS only)
#' occ_GoV <- get_site_speciesOccurrences(
#'   deimsid = "https://deims.org/758087d7-231f-4f07-bd7e-6922e0c283fd",
#'   list_DS = "obis",
#'   show_map = FALSE,
#'   limit = 10
#' )
#' occ_GoV
#'
#' # Marine site: Gulf of Venice (all sources excluding records of GBIF sourced from iNaturalist)
#' occ_GoV_all <- get_site_speciesOccurrences(
#'   deimsid = "https://deims.org/758087d7-231f-4f07-bd7e-6922e0c283fd",
#'   list_DS = c("gbif", "inat", "obis"),
#'   show_map = TRUE,
#'   limit = 10,
#'   exclude_inat_from_gbif = TRUE
#' )
#' occ_GoV_all$obis
#' occ_GoV_all$map
#' }
#'
#' @section The function output:
#' \figure{get_site_speciesOccurrences_fig.png}{Map of occurrences acquired
#' from OBIS in the marine site Gulf of Venice}
#'
### function get_site_speciesOccurrences
get_site_speciesOccurrences <- function(
    deimsid,
    list_DS,
    show_map = TRUE,
    limit = 500,
    exclude_inat_from_gbif = TRUE
) {
  # --- Check optional packages ---
  for (pkg in c("spocc", "RColorBrewer")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "\n----\nThe function 'get_site_speciesOccurrences()' requires the optional package '", pkg, "'.\n",
        "Please install it with: install.packages(\"", pkg, "\")\n----\n"
      )
    }
  }
  
  # --- Get site boundary ---
  boundary <- get_site_info(
    deimsid = deimsid,
    categories = c("General", "EnvCharacts"),
    show_map = FALSE
  )
  
  if (is.null(boundary) || !inherits(boundary, "sf")) {
    message("\n----\nNo boundary found for DEIMS ID: ", deimsid, "\n----\n")
    return(invisible(NULL))
  }
  
  # --- eLTER intersection fields ---
  elter_fields <- c(
    "title.x", "uri", "created", "changed", "geoCoord", "country",
    "geoElev.avg", "geoElev.min", "geoElev.max",
    "biogeographicalRegion", "biome", "ecosystemType",
    "eunisHabitat", "landforms", "geoBonBiome",
    "geology", "hydrology", "soils", "vegetation", "size.value"
  )
  
  keep_boundary <- intersect(elter_fields, names(boundary))
  boundary <- boundary |>
    dplyr::select(dplyr::all_of(keep_boundary), geometry) |>
    dplyr::rename_with(
      .fn   = ~ paste0("eLTER_", .x),
      .cols = dplyr::all_of(keep_boundary)
    )
  
  bbox_wkt <- sf::st_as_text(sf::st_as_sfc(sf::st_bbox(boundary)))
  
  # --- Download occurrences ---
  occ_fx <- getExportedValue("spocc", "occ")
  obis_search_fx <- getExportedValue("spocc", "obis_search")
  
  site_occ_spocc <- NULL
  site_occ_spocc_obis <- NULL
  
  if (any(c("gbif", "inat") %in% list_DS)) {
    site_occ_spocc <- occ_fx(
      from       = list_DS[list_DS %in% c("gbif", "inat")],
      geometry   = bbox_wkt,
      limit      = limit,
      has_coords = TRUE
    )
  }
  
  if ("obis" %in% list_DS) {
    site_occ_spocc_obis <- obis_search_fx(
      size     = limit,
      geometry = bbox_wkt
    )
  }
  
  # --- Helper: harmonise datetime columns ---
  .fix_datetime <- function(df, cols_datetime, cols_date = NULL) {
    for (col in cols_datetime) {
      if (!is.null(df[[col]])) {
        df[[col]] <- lubridate::as_datetime(df[[col]])
      }
    }
    for (col in cols_date) {
      if (!is.null(df[[col]])) {
        df[[col]] <- lubridate::as_date(df[[col]])
      }
    }
    df
  }
  
  if ("gbif" %in% list_DS && !is.null(site_occ_spocc$gbif$data[[1]])) {
    site_occ_spocc$gbif$data[[1]] <- .fix_datetime(
      site_occ_spocc$gbif$data[[1]],
      cols_datetime = c("lastCrawled", "lastParsed", "dateIdentified", "modified"),
      cols_date     = "eventDate"
    )
  }
  
  if ("inat" %in% list_DS && !is.null(site_occ_spocc$inat$data[[1]])) {
    site_occ_spocc$inat$data[[1]] <- .fix_datetime(
      site_occ_spocc$inat$data[[1]],
      cols_datetime = c("time_observed_at", "created_at", "updated_at")
    )
  }
  
  if ("obis" %in% list_DS && !is.null(site_occ_spocc_obis$results)) {
    site_occ_spocc_obis$results <- .fix_datetime(
      site_occ_spocc_obis$results,
      cols_datetime = "modified"
    )
  }
  
  # --- Helper: convert to sf and intersect with site boundary ---
  .to_sf_and_intersect <- function(df, lon_col, lat_col, source_name = "") {
    
    no_occ_msg <- function() message(
      "No occurrences found within the boundary of eLTER site '",
      boundary$eLTER_title.x, "' for: ", toupper(source_name)
    )
    
    if (is.null(df) || nrow(df) == 0L) { no_occ_msg(); return(NULL) }
    
    df[[lon_col]] <- as.numeric(df[[lon_col]])
    df[[lat_col]] <- as.numeric(df[[lat_col]])
    df <- df[!is.na(df[[lon_col]]) & !is.na(df[[lat_col]]), ]
    
    if (nrow(df) == 0L) { no_occ_msg(); return(NULL) }
    
    sf_obj        <- sf::st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326)
    original_cols <- setdiff(names(sf_obj), "geometry")
    boundary_4326 <- sf::st_transform(boundary, crs = 4326)
    sf_obj        <- suppressWarnings(sf::st_intersection(x = sf_obj, y = boundary_4326))
    
    if (nrow(sf_obj) == 0L) { no_occ_msg(); return(NULL) }
    
    sf_obj |>
      dplyr::select(
        dplyr::any_of(original_cols),
        dplyr::starts_with("eLTER_")
      )
  }
  
  # --- Build output list ---
  occ_list <- list(gbif = NULL, inat = NULL, obis = NULL)
  
  if ("gbif" %in% list_DS && !is.null(site_occ_spocc$gbif$data[[1]])) {
    df_gbif <- site_occ_spocc$gbif$data[[1]]
    if (exclude_inat_from_gbif && "inat" %in% list_DS) {
      df_gbif <- dplyr::filter(df_gbif, institutionCode != "iNaturalist")
    }
    occ_list$gbif <- .to_sf_and_intersect(df_gbif, "longitude", "latitude", "gbif")
  }
  
  if ("inat" %in% list_DS && !is.null(site_occ_spocc$inat$data[[1]])) {
    occ_list$inat <- .to_sf_and_intersect(
      site_occ_spocc$inat$data[[1]], "longitude", "latitude", "inat"
    )
  }
  
  if ("obis" %in% list_DS && !is.null(site_occ_spocc_obis$results) &&
      nrow(site_occ_spocc_obis$results) > 0L) {
    occ_list$obis <- .to_sf_and_intersect(
      site_occ_spocc_obis$results, "decimalLongitude", "decimalLatitude", "obis"
    )
  }
  
  # Remove NULL slots (sources with no data)
  occ_list <- Filter(Negate(is.null), occ_list)
  
  if (length(occ_list) == 0L) {
    message("\n----\nNo occurrences found within the site boundary.\n----\n")
    return(invisible(NULL))
  }
  
  # --- Optional map ---
  # --- Build map always, print only if show_map = TRUE ---
  if (length(occ_list) > 0L) {
    brewer.pal_fx <- getExportedValue("RColorBrewer", "brewer.pal")
    n_sources     <- length(occ_list)
    my_palette    <- brewer.pal_fx(max(3, n_sources), "Set1")[seq_len(n_sources)]
    factpal       <- leaflet::colorFactor(my_palette, levels = names(occ_list))
    
    occ_map <- leaflet::leaflet() |>
      leaflet::addProviderTiles("CartoDB.PositronNoLabels", group = "Basemap") |>
      leaflet::addTiles("http://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}.png")
    
    for (src in names(occ_list)) {
      coords  <- sf::st_coordinates(occ_list[[src]])
      df_plot <- as.data.frame(coords)
      df_plot$name <- if ("eLTER_title.x" %in% names(occ_list[[src]])) {
        occ_list[[src]]$eLTER_title.x
      } else {
        src
      }
      df_plot$prov <- src
      
      occ_map <- occ_map |>
        leaflet::addCircleMarkers(
          data        = df_plot,
          lat         = ~Y,
          lng         = ~X,
          popup       = ~name,
          radius      = 3,
          weight      = 2,
          opacity     = 0.5,
          fill        = TRUE,
          fillOpacity = 0.2,
          color       = factpal(src),
          group       = src
        )
    }
    
    occ_map <- occ_map |>
      leaflet::addLegend(
        position = "bottomright",
        pal      = factpal,
        values   = names(occ_list),
        opacity  = 1,
        title    = "Occurrence source"
      ) |>
      leaflet::addLayersControl(
        overlayGroups = names(occ_list),
        options       = leaflet::layersControlOptions(collapsed = FALSE)
      )
    
    # Always attach map to output; print only if requested
    occ_list$map <- occ_map
    if (isTRUE(show_map)) print(occ_map)
  }
  
  occ_list
}
