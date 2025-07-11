#' Acquire various raster layers from
#' \href{https://ecodatacube.eu/}{EcoDataCube EU}
#' and crop to an eLTER site boundary.
#' @description `r lifecycle::badge("stable")`
#' Download and return a SpatRaster object containing the requested
#' dataset from \href{https://ecodatacube.eu/}{EcoDataCube EU},
#' cropped to an eLTER site boundary, which is obtained from the DEIMS-SDR API.
#' @param deimsid  `string`. The DEIMS ID of the site from
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
#' @param dataset_year `string` indicating which year to choose (for multi-year datasets)
#' Entered as four digits (i.e. '2020')
#' @param dataset_month `string` indicating which month (for multi-month datasets).
#' Entered as two digits. i.e. '02', '03', or '11' etc.
#' @param show_map `Bool` whether to show `leaflet` map of downloaded dataset.
#' Default TRUE
#' @param output_dir `string` where to save the EcoDataCube dataset and style file.
#' The *.tif,  *.sld and *.qml files will be saved to the specified directory, 
#' with filename as <chosen_dataset>_<year><month>.tif 
#' Default tempdir()
#' @details Supported datasets from the EcoDataCube repository include:
#' \tabular{llcr}{
#'  dataset       \tab full name                                \tab date required  \tab temporal extent                  \cr
#'  ------------- \tab ---------------------------------------- \tab ---------------\tab -------------------------------  \cr
#'  CHELSA_precip \tab CHELSA Monthly accumulated precipitation \tab yes      \tab 2000-01-01 00:00:00 UTC–2019-06-30 00:00:00 UTC  \cr
#'  clc_2017      \tab Corine Landcover (CLC+) 2017-2019        \tab no       \tab 2017-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC  \cr
#'  clc_2020      \tab Corine Landcover (CLC+) 2020-2022        \tab no       \tab 2017-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC  \cr
#'  crop_map      \tab EUCROPMAP Pan-EU year 2022               \tab no       \tab 2022-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC  \cr
#'  DTM_30m       \tab OpenLandMap Ensemble Digital Terrain Model \tab no  \tab 2006-01-01 00:00:00 UTC–2015-12-31 00:00:00 UTC     \cr
#'  MODIS_LST_day \tab MOD11A2 monthly land surface temp. (day)   \tab yes \tab 2000-01-01 00:00:00 UTC–2021-12-31 00:00:00 UTC     \cr
#'  MODIS_LST_night\tab MOD11A2 monthly land surface temp. (night)\tab yes \tab 2000-01-01 00:00:00 UTC–2021-12-31 00:00:00 UTC     \cr
#'  NDVI_bimonthly \tab Cloud-free reconstructed Landsat bimonthly NDVI \tab yes \tab 2000-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC \cr
#'  NDVI_yearly   \tab Cloud free reconstructed yearly Landsat NDVI     \tab yes \tab 2000-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC \cr
#'  NDWI_bimonthly  \tab Cloud free reconstructed bi-monthly NDWI (Gao)   \tab yes \tab 2000-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC \cr
#'  soil_type     \tab AI4SoilHealth: Soil type dominant class          \tab no  \tab 2000-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC \cr
#'  }

#' All datasets are georeferenced to the
#' EPSG:3035 coordinate reference system.
#' 
#' @return The function returns a SpatRaster object (from the `terra` package)
#' of the requested dataset, cropped to the site boundaries. 
#' The SpatRaster is also saved as GeoTiff, along with the style files,
#' in the chosen `output_dir`.
#' 
#' @author Micha Silver, phD (2020) \email{silverm@@post.bgu.ac.il}
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom dplyr case_when
#' @importFrom sf st_transform
#' @importFrom terra mask crop vect rast crs plot
#' @importFrom leaflet addRasterImage
#' @importFrom utils download.file read.csv
#' @references
#'   \insertRef{dplyrR}{ReLTER}
#'
#'   \insertRef{terraR}{ReLTER}
#' @export
#' @examples
#' # Example of TERENO Harsleben
#' deimsid = "https://deims.org/c945abe4-3d40-46d1-b5d0-33127c35c6ab"
#' # NDVI
#' harsleben_ndvi <- get_site_EcoDataCube(
#'   deimsid = deimsid,
#'   dataset = "NDVI_bimonthly",
#'   dataset_year = "2021",
#'   dataset_month = "06"
#' )
#' # EUCrop map
#' harsleben_crop <- get_site_EcoDataCube(
#'   deimsid = deimsid,
#'   dataset = "crop_map",
#' )
#' # MODIS Land Surface Temperature, 
#' # Kalkalpen National Park - Austria
#' deimsid = "https://deims.org/49515dda-1198-4013-8f43-c33e107af081"
#' kalkalpen_LST_day <- get_site_EcoDataCube(
#'   deimsid = deimsid,
#'   dataset = "MODIS_LST_day",
#'   dataset_year = "2010",
#'   dataset_month = "01"
#' )
#' 
#' @md

### function get_site_EcoDataCube()
get_site_EcoDataCube <- function(deimsid, dataset = "",
                                 dataset_year = NA,
                                 dataset_month = NA,
                                 show_map = TRUE,
                                 output_dir = tempdir()) {
  
  edc_df <- read.csv(system.file("extdata/ecodatacube.csv",
                                 package = "ReLTER"))

  if (is.na(dataset) | dataset == "") {
    knitr::kable(select(edc_df, c(1,2,4,5,6,7)))
    return(NULL)
  }
  # First check that site has a boundary
  boundary <- get_site_boundaries(
    deimsid = deimsid,
    with_locations = TRUE,
    show_map = FALSE
  )$data
  if (is.null(boundary) || !inherits(boundary, "sf")) {
    print("No boundary for requested DEIMS site.")
    return(NULL)
  }

  edc_row <- edc_df[edc_df$dataset == dataset,]
  
  # Validate date inputs
  if (edc_row$date_required) {
    # Make sure from_yr and from_mon are correctly entered
    extent_from <- lubridate::as_date(edc_row$temporal_extent_from)
    extent_to <- lubridate::as_date(edc_row$temporal_extent_to)
    if (as.numeric(dataset_year) < lubridate::year(extent_from) | 
        as.numeric(dataset_year) > lubridate::year(extent_to)) {
      message("Year parameter is beyond the temporal extent of the dataset.\n",
              "Please check parameters. Exiting...")
      return(NULL)
    }
    if (as.numeric(dataset_month) < 1 | as.numeric(dataset_month) > 12) {
      message("Month parameter is beyond the range '1' to '12'.\n",
              "Please check parameters. Exiting...")
      return(NULL)
    }
  }
  # Construct full URL
  full_url <- EDC_construct_full_url(edc_row, dataset_year, dataset_month)
  
  # terra::rast can address a virtual dataset *without* downloading
  ds <- terra::rast(full_url)
  if (is.null(ds) || !inherits(ds, "SpatRaster")) {
    print("No raster dataset downloaded")
    return(NULL)
  }
  # Crop and mask the raster dataset to the boundary polygon
  # The boundary must be transformed first
  # to the European CRS (EPSG:3035) used by EcoDataCube
  bound_v <- terra::vect(sf::st_transform(boundary, terra::crs(ds)))
  # Now vrt dataset is actually downloaded
  ds_site <- terra::mask(terra::crop(ds, bound_v), bound_v)
  tif_file <- paste0(
    stringr::str_to_lower(dataset),
    "_", dataset_year, dataset_month, ".tif")
  terra::writeRaster(ds_site,
                     filename = file.path(output_dir, tif_file),
                     overwrite = TRUE)
 
  sld_filename <- paste0(
    stringr::str_to_lower(edc_row$dataset), 
    "_", dataset_year, dataset_month, ".sld")
  sld_path <- file.path(output_dir, sld_filename)
  url_prefix <- "https://s3.ecodatacube.eu/arco/"
  sld_url <- paste0(url_prefix, edc_row$sld_url)
  download.file(sld_url, destfile = sld_path)
  
  qml_filename <- paste0(
    stringr::str_to_lower(edc_row$dataset),
    "_", dataset_year, dataset_month, ".qml"
  )
  qml_path <- file.path(output_dir, qml_filename)
  qml_url <- gsub(pattern = "sld", replacement = "qml",
                  x = edc_row$sld_url)
  qml_url <- paste0(url_prefix, qml_url)
  download.file(qml_url, destfile = qml_path)
  
  if (show_map) {
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::addPolygons(data = boundary,
                           color = "white",
                           weight = 3,
                           opacity = 1,
                           fill = FALSE) |>
      leaflet::addRasterImage(ds_site)
  }
  return(ds_site)
}

EDC_construct_full_url <- function(edc_row, dataset_year, dataset_month) {
  #' Construct full url for download
  #' @description Construct full URL,
  #' including replacing year and month where required.
  #' @param edc_row `vector` one row for chosen dataset from edc_df.
  #' @param dataset_year `string` Chosen year
  #' @param dataset_month `string` Chosen month
  #' @author Micha Silver, phD (2020) \email{silverm@@post.bgu.ac.il}
  #'
  # Replace {from} and {to} with dates, when needed...
  if (edc_row$date_required) {
    # Make sure month is two characters
    fr_mon <- stringr::str_pad(dataset_month, 2, pad = 0)
    fr_yr <- as.character(dataset_year)
    switch(edc_row$period,
           monthly = {
             from_date = lubridate::as_date(paste(
               fr_yr, fr_mon, "01", sep="-"))
             to_date = lubridate::ceiling_date(from_date, 'month') - lubridate::days(1)
             },
           bimonthly = {
             fr_mon <- as.integer(fr_mon)
             if ((fr_mon %% 2) == 0) {
               # This is an even month, Bimonthly only for odd numbered months,
               # so subtract 1
               fr_mon <- fr_mon - 1
               fr_mon <- stringr::str_pad(fr_mon, 2, pad = 0)
             }
             
              from_date = lubridate::as_date(paste(
                fr_yr, fr_mon, "01", sep="-"))
              to_date = from_date + months(2) - lubridate::days(1)
             },
           yearly = {
             from_date = lubridate::as_date(paste(fr_yr,"-01-01"))
             to_date  = lubridate::ceiling_date(from_date, "year") - lubridate::days(1)
           }
    )
    from_date_str <- strftime(from_date, "%Y%m%d")
    to_date_str <- strftime(to_date, "%Y%m%d")
    url <- gsub(pattern = "{from}",
                replacement = from_date_str,
                x = edc_row$url, fixed = TRUE)
    url <- gsub(pattern = "{to}", 
                replacement = to_date_str,
                x = url, fixed = TRUE)

  } else {
    url <- edc_row$url
  }
  url_prefix <- "https://s3.ecodatacube.eu/arco/"
  full_url <- paste0("/vsicurl/", url_prefix, url)
  
  return(full_url)
}
