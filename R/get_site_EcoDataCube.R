#' Acquire various raster layers from
#' \href{https://ecodatacube.eu/}{EcoDataCube EU}
#' and crop to an eLTER site boundary.
#' @description `r lifecycle::badge("stable")`
#' Download and return a SpatRaster object containing the requested
#' dataset from \href{https://ecodatacube.eu/}{EcoDataCube EU},
#' cropped to an eLTER site boundary, which is obtained from the DEIMS-SDR API.
#' @param deimsid  A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @param dataset A `character`. The requested dataset. One of:
#' "crop_map","clc_2017", "clc_2020","chelsa_precipitation","NDVI_yearly",
#' "NDWI monthly", "DTM_30m", "MOD11A2_day", "MOD11A2_night"
#' Default is "".
#' @param year A `character` indicating which year to choose (for multi-year datasets)
#' @param month A `character` indicating which month (for multi-month datasets)
#' @param show_map Bool whether to show plot of downloaded raster
#' @details Supported datasets from the EcoDataCube repository include:

#' All datasets are georeferenced to the
#' EPSG:3035 coordinate reference system.
#' and all except clc2018 have 30 meters resolution
#' 
#' If dataset is NA or an empty string, then 
#' a list of the available EcoDataCube layers is printed.
#' 
#' @return The function returns a SpatRaster object (from the `terra` package)
#' of the requested dataset, cropped to the site boundaries
#' The user should save the raster to disk, if necessary.
#' i.e. writeRaster(ds_site, "site_dataset.tif")
#' @author Micha Silver, phD (2020) \email{silverm@@post.bgu.ac.il}
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom dplyr case_when
#' @importFrom sf st_transform
#' @importFrom terra mask crop vect rast crs plot
#' @references
#'   \insertRef{dplyrR}{ReLTER}
#'
#'   \insertRef{terraR}{ReLTER}
#' @export
#' @examples
#'  \dontrun{
#' # Landcover for Angelo Mosso
#' siteLandcover <- get_site_ODS(
#'   deimsid = "https://deims.org/17210eba-d832-4759-89fa-9ff127cbdf6e",
#'   dataset = "landcover"
#' )
#' siteLandcover
#' terra::plot(siteLandcover)
#'
#' # NDVI for Eisenwurzen
#' siteNDVI <- get_site_ODS(
#'   deimsid = "https://deims.org/d0a8da18-0881-4ebe-bccf-bc4cb4e25701",
#'   dataset = "ndvi_summer"
#' )
#' siteNDVI
#' terra::plot(siteNDVI)
#' }
#'
#' @section The function output:
#' \figure{get_site_ods_fig.png}{NDVI for Eisenwurzen}
#'
### function get_site_EcoDataCube()
get_site_EcoDataCube <- function(deimsid, dataset = "",
                                 year = NA, month = NA,
                                 show_map = TRUE) {
  edc_layers <- read.csv(system.file("extdata/ecodatacube.eu.csv",
                                     package = "ReLTER"))
  url_prefix <- "https://s3.ecodatacube.eu/arco/"
  if (is.na(dataset) | dataset == "") {
    knitr::kable(select(edc_layers, !url))
    return(NULL)
  }
  # First check that site has a boundary
  boundary <- ReLTER::get_site_info(
    deimsid,
    category = "Boundaries"
  )
  if (is.null(boundary) || !inherits(boundary, "sf")) {
    print("No boundary for requested DEIMS site.")
    return(NULL)
  }
  
  # Construct full URL
  # Replace {from} and {to} with dates, when needed...
  full_url <- paste0("/vsicurl/", url_prefix, dataset)
  
  
  # terra::rast can address a virtual dataset *without* downloading
  ds <- terra::rast(full_url)
  if (is.null(ds) || !inherits(ds, "SpatRaster")) {
    print("No raster dataset downloaded")
    return(NULL)
  }
  # Crop and mask the raster dataset to the boundary polygon
  # The boundary must be transformed first
  # to the European CRS (EPSG:3035) used by ODS
  boundary <- sf::st_transform(boundary, terra::crs(ds))
  bound_v <- terra::vect(boundary)
  ds_site <- terra::mask(terra::crop(ds, bound_v), bound_v)
  
  # Apply color table and labels if needed...
  sld_url <- paste0(url_prefix, edc_layers$sld_url)
  ds_colored <- apply_color_table(sld_url, ds_site)
  
  if (show_map) {
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::addPolygons(data = boundary,
                           color = "white",
                           weight = 3,
                           opacity = 1,
                           fill = FALSE) |>
      leaflet::addRasterImage(ds_colored)
  }
  return(ds_colored)
}

apply_color_table <- function(sld_url, r) {
  #' @description Download SLD style file and apply color table to raster
  #' @param sld_url Character full path to SLD file from EcoDataCube 
  #' @param r terra::rast The raster to apply color table
  #' @author Micha Silver, phD (2020) \email{silverm@@post.bgu.ac.il}
  #' 
  styles.sld <- file.path(tempdir(), "styles.sld")
  download.file(
    "https://s3.ecodatacube.eu/arco/soil.types_ensemble_hardclass.sld",
    destfile = styles.sld)
  
  sld <- xml2::read_xml(styles.sld)
  entries <- xml2::xml_find_all(sld, "//sld:ColorMapEntry")
  entry_list <- lapply(1:length(entries), function(e){
    ent <- entries[[e]]
    quan <- xml2::xml_attr(ent, 'quantity')
    lbl <- xml2::xml_attr(ent, 'label')
    clr <- xml2::xml_attr(ent, 'color')
    e_df <- data.frame(Quantity = as.numeric(quan),
                       Label = lbl, Color = clr)
    return(e_df)
  })
  entries_df <- do.call(rbind, entry_list)
  clrs_df <- dplyr::select(entries_df, c("Quantity", "Color"))
  lvls_df <- dplyr::select(entries_df, c("Quantity", "Label"))
  terra::coltab(r) <- clrs_df
  terra::levels(r) <- lvls_df
  return(r)
}