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
                                 year = NA, month = NA) {
  edc_layers <- read.csv(system.file("extdata/ecodatacube.eu.csv",
                                     package = "ReLTER"))
  if (is.na(dataset) | dataset == "") {
    knitr::kable(select(edc_layers, !url))
  }
}