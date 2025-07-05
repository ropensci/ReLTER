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
#' @param dataset `string` The requested dataset. One of:
#' "CHELSA_precip", "clc_2017", "clc_2020", "crop_map", "DTM_30m",
#' "MODIS_LST_day", "MODIS_LST_night", "NDVI_bimonthly", "NDVI_yearly",
#' "NDWI_monthly", "soil_type"
#' If dataset is NA or an empty string, then 
#' a list of the available EcoDataCube layers is printed.
#' Default is "".
#' (See Details for explanation of each dataset)
#' @param year `string` indicating which year to choose (for multi-year datasets)
#' @param month `string` indicating which month (for multi-month datasets)
#' @param show_map Bool whether to show plot of downloaded raster
#' @details Supported datasets from the EcoDataCube repository include:
#' \tabular{llcr}{
#'  short_name    \tab full name                                      \tab temporal extent             \tab  res. \cr
#'  --------------\tab ---------------------------------------------- \tab --------------------------- \tab ----- \cr
#'  CHELSA_precip \tab CHELSA Monthly accumulated precipitation       \tab 2000-01-01 00:00:00 UTC–2019-06-30 00:00:00 UTC \tab 1000 \cr
#'  clc_2017      \tab Corine Landcover (CLC+) 2017-2019              \tab 2017-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC \tab 10   \cr
#'  clc_2020      \tab Corine Landcover (CLC+) 2020-2022              \tab 2017-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC \tab 10   \cr
#'  crop_map      \tab EUCROPMAP Pan-EU year 2022                     \tab 2022-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC \tab 10   \cr
#'  DTM_30m       \tab OpenLandMap Ensemble Digital Terrain ModelM30) \tab 2006-01-01 00:00:00 UTC–2015-12-31 00:00:00 UTC \tab 30   \cr
#'  MODIS_LST_day \tab MOD11A2 monthly land surface temp. (day)       \tab 2000-01-01 00:00:00 UTC–2021-12-31 00:00:00 UTC \tab 1000 \cr
#'  MODIS_LST_night\tab MOD11A2 monthly land surface temp. (night)     \tab 2000-01-01 00:00:00 UTC–2021-12-31 00:00:00 UTC \tab 1000 \cr
#'  NDVI_bimonthly \tab Cloud-free reconstructed Landsat bimonthly NDVI\tab 2000-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC \tab 30   \cr
#'  NDVI_yearly   \tab Cloud free reconstructed yearly Landsat NDVI   \tab 2000-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC \tab 30    \cr
#'  NDWI_monthly  \tab Cloud free reconstructed bi-monthly NDWI (Gao) \tab 2000-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC \tab 30    \cr
#'  soil_type     \tab AI4SoilHealth: Soil type dominant class        \tab 2000-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC \tab 30    \cr
#'  }

#' All datasets are georeferenced to the
#' EPSG:3035 coordinate reference system.
#' 
#' @return The function returns a SpatRaster object (from the `terra` package)
#' of the requested dataset, cropped to the site boundaries.
#' If the SpatRaster is categorical, it will contain a colortable (from EcoDataCube).
#' The user should save the raster to disk, if necessary.
#' i.e.
#' 
#' `writeRaster(ds_site, "site_dataset.tif")`
#' 
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
#' }
#' @md

### function get_site_EcoDataCube()
get_site_EcoDataCube <- function(deimsid, dataset = "",
                                 year = NA, month = NA,
                                 show_map = TRUE) {
  edc_df <- read.csv(system.file("extdata/ecodatacube.csv",
                                     package = "ReLTER"))
  url_prefix <- "https://s3.ecodatacube.eu/arco/"
  if (is.na(dataset) | dataset -- "") {
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
  edc_row <- edc_df[edc_df$short_name == dataset,]
  # Construct full URL
  full_url <- EDC_construct_full_url(edc_row)
  
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
  ds_colored <- EDC_apply_color_table(sld_url, ds_site)
  
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

EDC_apply_color_table <- function(sld_url, r) {
  #' Apply color table, and categories to raster from SLD file
  #' @description Download SLD style file and apply color table to raster
  #' @param sld_url Character full path to SLD file from EcoDataCube 
  #' @param r terra::rast The raster to apply color table
  #' @author Micha Silver, phD (2020) \email{silverm@@post.bgu.ac.il}
  #' 
  styles.sld <- file.path(tempdir(), "styles.sld")
  download.file(sld_url, destfile = styles.sld)
  
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

EDC_construct_full_url <- function(edc_row, from_yr, from_mon) {
  #' Construct full url for download
  #' @description Construct full URL, including replaceing year and month where required.
  #' @param edc_row Vector, one row for chosen dataset from edc_df.
  #' @param from_yr Character Chosen year 
  #' @param from_mon Character Chosen month
  #' @author Micha Silver, phD (2020) \email{silverm@@post.bgu.ac.il}
  #' 
  # Replace {from} and {to} with dates, when needed...
  if (edc_row$date_required) {
    fr <- paste0(from_yr, from_mon, "01")
    url <- gsub(pattern = "{from}", replacement = fr, x = edc_row$url)
  } else {
    url <- edc_row$url
  }
  full_url <- paste0("/vsicurl/", edc_row$url)
  
  return(url)
}