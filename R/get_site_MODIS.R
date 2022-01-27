#' @title eLTER get_site_MODIS function
#' @description This function acquires MODIS products
#' from MODIS Land Products archive,
#' crops to an eLTER site boundary, and
#' plots a time series of the MODIS derived variable averaged over the eLTER site.
#'
#' @param deimsid  a `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. More information about DEIMS ID from:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @param product a `character`. The requested MODIS product.
#' Must be the full product name.
#' Use `show_products = TRUE` to get a list of products.
#' Some examples:
#' 	  "Vegetation_Indexes_Monthly_1Km (M*D13A3)"
#' 		"LST_3band_emissivity_8day_1km (M*D21A2)"
#'		"LAI_8Days_500m (MCD15A2H)"
#' Default is "Vegetation_Indexes_Monthly_1Km (M*D13A3)"
#' @param bands `character`, string array of bands in the requested product.
#' Use the "show_bands" parameter first to get a list of available bands.
#' Some examples: "LST_Day_1km", or multiple bands as c("NDVI", "EVI")
#' Default is c("NDVI", "EVI")  
#' @param scale `boolean`. The MODIS raster values will be scaled (multiplied)
#' by a factor. Some MODIS products are scaled up and saved as 16 bit integers.
#' For example, Vegetation indices are scaled by 10000,
#' and Land surface temperatures are scaled by 50.
#' Scaling factors are derived from product metadata.
#' For additional details refer to the  MODIS Product Tables: 
#' https://modis.gsfc.nasa.gov/data/dataprod/
#' Default TRUE (scaling applied)
#' @param from_date, `character`, the start date for acquiring MODIS
#' formatted as YYYY.MM.DD
#' Must be later than 2000.02.01. (The earliest MODIS imagery was Feb 2000)
#' @param to_date, `character`, the last date for acquiring MODIS
#' formatted as YYYY.MM.DD.
#' @param out_folder, `character`, Where to save the downloaded MODIS rasters
#' Default is tempdir(). 
#' (Note that if this parameter is left at default the downloaded MODIS rasters
#' will be deleted on exit, along with other temporary R files.)
#' @param save_ts_dir, `character`. Full path to directory to save a CSV file
#' of the time series of site averaged MODIS bands values.
#' Default is NA (saving is disabled). 
#' This directory is created if it does not exist.
#' @param earthdata_user `character`. For authentication. 
#' Please create an account on: https://urs.earthdata.nasa.gov/users/new
#' and enter authentication details here.
#' @param earthdata_passwd `character` For authentication on Earthdata.
#' @param show_products, a `boolean`, if TRUE, then show a list of all
#' available MODIS products, and exit. Default is FALSE.
#' @param show_bands, a `character`, the name of a MODIS product.
#' The parameter could be one of: 
#'     "M*D13Q1", (Vegetation Indexes_16Days_250m),
#'     "M*D21A2", (LST_3band_emissivity_8day_1km),
#'     "MCD12Q1". (LandCover_Type_Yearly_500m)
#' When this parameter is supplied, show a list of the bands
#' for that product, and exit. Default is NA.
#'
#' @details The Nasa [EarthData](https://urs.earthdata.nasa.gov/home) site 
#' offers MODIS products as 8 day, 16 day and  monthly aggregations. 
#' This function accesses those aggregations
#' selecting a subset from the `from_date` to the `to_date`,
#' then prepares site averaged values of the MODIS product, 
#' and plots a time series of those averages.
#' In order to download from EarthData, you must register at
#' https://urs.earthdata.nasa.gov/users/new
#' and supply your username and password to this function.
#' 
#' @return The function returns a SpatRaster object (from the `terra` package)
#' which contains a stack of all rasters for the requested product/bands
#' over the requested date interval.
#' The user should save the raster stack to disk, if necessary.
#' i.e. writeRaster(ds_site, "site_MODIS_dataset.tif")
#' The function also plots a time series graph of the site averaged values.

#' @author Micha Silver, phD (2020) \email{silverm@@post.bgu.ac.il}
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom sf st_transform st_bbox
#' @importFrom terra vect rast 
#' @import MODIStsp
#' @export
#' @examples
#'  \dontrun{
#'  deimsid = "https://deims.org/86e3a1ca-b2ba-4b06-b096-71447df52841"
#'  product_bands <- MODIStsp_get_prodlayers("M*D13Q1")
#'  print(product_bands[,6:7])
#' }

### function get_site_ODS
get_site_MODIS <- function(deimsid,
                           earthdata_user,
                           earthdata_passwd,
                           product="Vegetation_Indexes_Monthly_1Km (M*D13A3)", 
                           from_date = NA,
                           to_date = NA,
                           bands=c("NDVI", "EVI"),
                           scale = TRUE,
                           save_ts_dir = NA,
                           out_folder = tempdir(),
                           show_products = FALSE,
						               show_bands = NA) {

  ## Check inputs ##
  #-----------------------------#
  if (!is.na(show_bands) && is.character(show_bands)) {
    # prod_opt_list <- MODIStsp:::load_prodopts()
    # selprod <- grep(pattern = show_bands, x = names(prod_opt_list),
    #                 fixed = TRUE)
    # if (length(selprod) == 0) {
    #   stop("Invalid product name. Aborting! \"show_bands\" should be a MODIS
    #      product code (e.g., \"M*D13Q1\"), or full MODIStsp product name
    #      (e.g., \"Vegetation Indexes_16Days_250m (M*D13Q1)\")")
    # } else {
    # If the show_bands string is not in products list,
    # the MODIStsp_get_prodbands() will throw an error
    product_bands <- MODIStsp::MODIStsp_get_prodlayers(show_bands)
    if (is.list(product_bands)) {
      print(product_bands$bandnames)
    }
    return(NULL)
  }
  
  if (show_products == TRUE) {
    MODIS_products <-  MODIStsp::MODIStsp_get_prodnames()
    print(MODIS_products)
    return(NULL)
  } 
  
  # Check that username and password are strings
	if (!is.character(earthdata_user) | !is.character(earthdata_passwd)) {
		stop("Missing username or password", call.=FALSE)
	}

  # Check that from_date and to_date are correct format
	CheckValidDate <- function(d1, d2) {
	  # Test for:
	  #     date format (YYYY.mm.dd)
	  #     from date not earlier than 2000/02/01
	  #     to_date later than from_date
		check_valid = regexpr("\\d\\d\\d\\d\\.[0-1]\\d\\.[0-3]\\d$",
	            						d1, perl=TRUE)
		check_valid = check_valid & 
		  regexpr("\\d\\d\\d\\d\\.[0-1]\\d\\.[0-3]\\d$", d2, perl=TRUE)
		check_valid = check_valid & 
		  !is.na(as.POSIXct(d1, format="%Y.%m.%d")) &
		  !is.na(as.POSIXct(d2, format="%Y.%m.%d"))
    check_valid = check_valid & 
      (as.POSIXct(d1, format="%Y.%m.%d") > "2000-02-01") &
      as.POSIXct(d1, format="%Y.%m.%d") < as.POSIXct(d2, format="%Y.%m.%d")
    return(check_valid)
	}
	
	if (CheckValidDate(from_date, to_date) == FALSE) {
	  stop("Either from_date: ", from_date, " or to_date: ", to_date,
	       " are not formatted correctly, or incorrect dates.
		     \n Please check dates and format as: YYYY.mm.dd", call.=FALSE) 
	}

  ## OK to proceed ##
  #-----------------------------#
  # Get site boundary for clipping MODIS
  boundary <- ReLTER::get_site_info(deimsid, "Boundaries")
  if (is.null(boundary)) {
    message("The site at DEIMS ID:", deimsid,
			"does not have a boundary.\n Exiting...")
    return(NULL)
  } else {
    # Call MODIStsp, Get MODIS bands
    bndry_bbox <- sf::st_bbox(boundary)
    MODIStsp::MODIStsp(gui = FALSE,
                       out_folder = out_folder,
					             prod_version = "006",
                       selprod = product,
                       bandsel = bands,
                       user = earthdata_user,
                       password = earthdata_passwd,
                       start_date = from_date,
                       end_date = to_date,
                       verbose = TRUE,
                       out_format = "GTiff",
                       compress = "LZW",
                       delete_hdf = TRUE,
          					   out_projsel= "User Defined",
                       output_proj = "4326",
					             out_res_sel = "Native",
                       spatmeth = "bbox",
                       bbox = bndry_bbox,
                       scale_val = scale,
                       n_retries = 20
                       )
  }


  ## Prepare time series plot ##
  #-----------------------------#
  out_files <- list.files(out_folder, pattern = ".tif$",
                          recursive = TRUE,  full.names = TRUE)
  # Make sure we're using only the files from the requested bands
  # (in case function is called again in same session)
  # Prepare regular expr with band names
  if (length(bands) > 1) {
    pat = paste0(bands, collapse="|")
  } else { pat = bands }
  out_files = out_files[grep(pattern=pat, out_files)]

  # Now load into stack and Mask to boundary
  modis_stk <- terra::rast(out_files)
  modis_stk <- terra::mask(modis_stk, terra::vect(boundary))

	# Loop thru bands if > 1, and create timeseries,
  # each band in separate panel
  modis_ts_list <- lapply(bands, function(b) {
    b_idx = grep(b, names(modis_stk))
    modis_band_stk <- modis_stk[[ b_idx ]]
    modis_band_ts <- terra::global(modis_band_stk, mean, na.rm = TRUE)
    # Get date from row names
    img_dates <- lapply(row.names(modis_band_ts), function(n) {
                         n_parts = unlist(strsplit(n, split="_")) 
                         yr_jday <- tail(n_parts, 2)
                         img_date = as.POSIXct(paste(yr_jday, collapse="-"),
                                                format = "%Y-%j")
                         return(img_date)
                         })
    image_dates = (Reduce(c, img_dates))
    modis_band_ts$image_date <- image_dates
    modis_band_ts$band <- b
    return(modis_band_ts)
  })

  ## Now plot ##
  #-----------------------------#
  # Prepare for multipanel plot
  num_plots <- length(modis_ts_list)
  dev.new(width=7, height=5*num_plots)
  par(mfcol = c(num_plots, 1))
  for (p in seq_along(1:num_plots)) {
    band_ts = modis_ts_list[[p]]
    # Get band name from first row (all are the same)
    b <- band_ts$band[1]
    # First get rid of any NaN's (in case *all* pixels were NA)
    # and put in order, in case both Aqua and Terra  MODIS platforms used
    band_ts = band_ts[complete.cases(band_ts),]
    band_ts = band_ts[order(band_ts$image_date),]
    fr_dte <- band_ts$image_date[1]
    to_dte <- band_ts$image_date[length(band_ts$image_date)]
    ttl <- paste("Time series", b,
                 fr_dte, "to", to_dte)
    plot(band_ts$image_date, band_ts$mean, type="l", col="blue",
         lwd=3, main=ttl, xlab = "Image date", ylab=b)
    
    # Save this band time series to CSV
    create_ok <- FALSE
    if (is.character(save_ts_dir) & !is.na(save_ts_dir)) {
      # Create directory and save modis_ts to CSV
      if (!dir.exists(save_ts_dir)) {
        create_ok <- dir.create(save_ts_dir, recursive = TRUE)
      }
    } else {  # No time series directory specified, save to out_folder
      save_ts_dir <- file.path(out_folder, "Time_Series")
      create_ok <- dir.create(save_ts_dir, recursive = TRUE)
    }
    if (dir.exists(save_ts_dir) | create_ok) {
        output_file = paste0(paste("MODIS", b, fr_dte, to_dte,
                                   sep="_"),
                             ".csv")
        output_csv = file.path(save_ts_dir, output_file)
        message("Saving time series to CSV file: ", output_csv)
        write.csv(band_ts, output_csv)
    } else {
        message("\nCould not write time series to: ", save_ts_dir, "\n")
    }
  }

  return(modis_stk)
}
