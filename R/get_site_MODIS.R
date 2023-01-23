#' Acquire a time series of MODIS satellite products
#' 

#' @description `r lifecycle::badge("stable")`
#' Acquire either Land Surface Temperature (LST) or Vegetation Index (NDVI)
#' both cropped to an eLTER site boundary. Download a timeseries of MODIS images containing the requested
#' product and optionally:
#' 
#' Plot a time series graph of the average values over the site.
#' 
#' Create and show an aggregated map of the acquired product
#' 
#' Use of this function requires registering on the EarthData website:
#' 
#'    https://urs.earthdata.nasa.gov/home
#
#' In order to guard your user credentials, please save
#' your username and password to environment variables. i.e.
#' 
#'  Sys.setenv("earthdata_user"="homer_simpson")
#'  
#'  Sys.setenv("earthdata_pass"="bart&lucy")
#' 
#' @param deimsid  `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @param product `character`. The requested product. One of: "LST", "VI", "ET", "LAI". 
#'     "LST" for Land Surface Temperature, night and day,
#'     8 day intervals at 1000m resolution
#'     "VI" for Vegetation Indices, NDVI and EVI
#'     16 day intervals at 250m resolution
#'     "LAI" for Leaf area index and FPAR at 500m resolution
#'     "ET" for Evapotranspiration, 8 day interval at 500m resolution
#' Default is "VI".
#' @param from_date `character`: the start date formatted as YYYY.MM.DD 
#' @param to_date `character`: the end date formatted as YYYY.MM.DD
#' @param output_dir `character`: where to save downloaded rasters (Default is `tempdir()`)
#' @param plot_ts `boolean`: whether to plot the time series, 
#' Default TRUE.
#' @param output_proj `character`: the EPSG code of desired output projection.
#' Default is "3035", the European LAEA coordinate reference system.
#' @param download_range `character`: one of "Full" or "Seasonal".
#' Specifies whether to acquire all images between start and end dates, or
#' only for a specific season. e.g. if the starting date is "2010.01.01"
#' and the ending date is "2020.02.28" then only images for
#' January and February are acquired, over the 10 year time span. (See example)
#' @param show_map `character`: Whether to create, save and display an
#' aggregated map from the time series of acquired MODIS products.
#' This string must be one of:
#' 
#'     FALSE (the default): no map is shown or created. 
#'     Otherwise: an aggregation function such as "mean", "max", or "min.
#'
#' @details
#' Certain layers from each of the supported MODIS products are acquired.
#' 
#' * from: "LST_3band_emissivity_8day_1km (M*D21A2)" two "Land surface temperature" bands are acquired:
#' 
#'     "LST_Day_1KM", "LST_Night_1KM"
#' 
#' * from: "Vegetation Indexes_16Days_250m (M*D13Q1)" two Vegetation Indicies
#' are acquired:
#' 
#'     "NDVI" and "EVI"
#'  
#' * from: "LAI_8Days_500m (M*D15A2H)" two indicies are acquired:
#' 
#'     "Fpar" and "Lai" 
#' 
#' * from: "Net_ET_8Day_500m (M*D16A2)" one Evapotranspiration band:
#' 
#'     "PET" (Potential EvapoTranspiration)
#'     
#' NOTES:
#' 
#' * The default `output_dir` is tempdir(), so the downloaded MODIS files will be deleted when exiting R.
#' Enter a permanent path for `output_dir` to save the files.
#' 
#' * Use the `plot_ts` parameter to create and save line plots of
#' a time series of average pixel values over the site.
#'
#' * Use the `show_map` parameter to create and show a time series 
#' aggregation map of the product over the site.
#' 
#' * Evapotranspiration products are available only up to 2018
#' 
#' @return Full path of all downloaded and cropped Geotiff files
#' 
#' @importFrom stats complete.cases
#' @author Micha Silver, phD (2020) \email{silverm@@post.bgu.ac.il}
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @references
#'   \insertRef{sfR}{ReLTER}
#'
#'   \insertRef{terraR}{ReLTER}
#'
#' MODIS images from:
#' https://lpdaac.usgs.gov, maintained by the NASA EOSDIS
#' Land Processes Distributed Active Archive Center (LP DAAC)
#' at the USGS Earth Resources Observation and Science (EROS) Center,
#' Sioux Falls, South Dakota. 2018,
#' https://lpdaac.usgs.gov/resources/data-action/aster-ultimate-2018-winter-olympics-observer/.
#' 
#' @export
#' @examples
#'  \dontrun{
#' # Lago Maggiore - Italy, LST over an 6 month time span
#' # Saved in LAEA ETRS89 coordinate reference system
#' # This example completes in about 10 mins
#' deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' product <- "LST"
#' from_date <- "2018.03.01"
#' to_date <- "2018.08.30"
#' output_dir <- tempdir()
#' output_proj <- "3035"
#' download_list <- ReLTER::get_site_MODIS(deimsid,
#'     product=product,
#'     from_date=from_date, to_date=to_date,
#'     output_dir=output_dir,
#'     plot_ts=TRUE,
#'     output_proj=output_proj)
#' 
#' # Northern Negev LTER - Israel, NDVI over 4 winter months,
#' # projected to Israeli 05/12 CRS
#' # This example completes in about 30 mins
#' deimsid <- "https://deims.org/871a90b2-e372-456a-93e3-518ad1e11239"
#' from_date <- "2018.01.01"
#' to_date <- "2018.04.30"
#' product="VI"
#' output_dir <- tempdir()
#' output_proj <- "6991"
#' download_list <- ReLTER::get_site_MODIS(deimsid,
#'     product=product,
#'     from_date=from_date, to_date=to_date,
#'     output_dir=output_dir,
#'     plot_ts=TRUE,
#'     output_proj=output_proj)
#'
#'  
#' # Nationalpark Mols Bjerge - Denmark, 10 year only for July 
#' # Show aggregated mean NDVI and EVI, (No time series plot)
#' # projected to EPSG:25832 (UTM zone 32, ETRS89)
#' # Takes about 3/4 hour to run...
#' deimsid <- "https://deims.org/8407da23-d75d-4a02-a5a5-7b9701a86743"
#' from_date <- "2005.07.01"
#' to_date <- "2015.08.01"
#' output_dir <- tempdir()
#' output_proj <- "25832"
#' product <- "VI"
#' download_list <- ReLTER::get_site_MODIS(deimsid,
#'     product=product,
#'     from_date=from_date, to_date=to_date,
#'     output_dir=output_dir,
#'     output_proj=output_proj,
#'     download_range="Seasonal",
#'     plot_ts=FALSE,
#'     show_map="mean")
#' 
#'
#' # Braila Islands - Romania, 2 year time series of evapotranspiration
#' # projected to Pulkova 1942(59) Zone 9 CRS, EPSG:3839
#' # Takes almost 1.5 hours to run (requires 2 MODIS tiles)
#' deimsid <- "https://deims.org/d4854af8-9d9f-42a2-af96-f1ed9cb25712"
#' from_date <- "2015.01.01"
#' to_date <- "2016.12.31"
#' output_dir <- tempdir()
#' output_proj <- "3839"
#' product <- "ET"
#' download_list <- ReLTER::get_site_MODIS(deimsid,
#'     product=product,
#'     from_date=from_date, to_date=to_date,
#'     output_dir=output_dir,
#'     output_proj=output_proj,
#'     download_range="Full",
#'     plot_ts=TRUE,
#'     show_map=FALSE)
#'     
#'     
#' # Gran Paradiso National Park - Italy, 1 year time series of LAI and aggregated map
#' # projected to ETRS89 LAEA, EPSG:3035
#' # Takes about 3/4 hour to run
#' deimsid <- "https://deims.org/e33c983a-19ad-4f40-a6fd-1210ee0b3a4b"
#' from_date <- "2020.01.01"
#' to_date <- "2020.12.31"
#' output_dir <- tempdir()
#' output_proj <- "3035"
#' product <- "LAI"
#' download_list <- ReLTER::get_site_MODIS(deimsid,
#'     product=product,
#'     from_date=from_date, to_date=to_date,
#'     output_dir=output_dir,
#'     output_proj=output_proj,
#'     download_range="Full",
#'     plot_ts=TRUE,
#'     show_map="mean")
#'
#' }
#'
#'
### function get_site_MODIS
get_site_MODIS <- function(deimsid, product = "VI",
                         from_date='2010.01.01', to_date='2020.31.12',
                         output_dir=NULL,
                         plot_ts=TRUE,
                         output_proj = "3035",
                         download_range="Full",
                         show_map=FALSE) {

  # Make sure the requested product is among those supported
  if (! product %in% c("VI", "LST", "ET", "LAI")) {
    stop(paste(product, "not supported"))
  }
  # Setup category and product for the chosen product
  prod_version="061"
  if (product == "VI") {
    prod <- 'Vegetation Indexes_16Days_250m (M*D13Q1)'
    categ <- "Ecosystem Variables - Vegetation Indices"
    bands <- c("NDVI", "EVI")
    scale_val <- TRUE
    output_res  <-  "250"
  } else if (product == "LST") { 
    prod <- "LST_3band_emissivity_8day_1km (M*D21A2)"
    categ <- "Radiation Budget Variables - Land Surface Temperature/Emissivity"
    bands <- c("LST_Day_1KM", "LST_Night_1KM")
    scale_val <- TRUE
    output_res <-  "1000"
  } else if (product == "LAI") { 
    prod <-  "LAI_8Days_500m (M*D15A2H)"
    categ <- "Ecosystem Variables - Vegetation Indices"
    bands <- c("Fpar", "Lai")
    scale_val <- TRUE
    output_res <-  "500"
  } else {  # "ET"
    prod <- "Net_ET_8Day_500m (M*D16A2)"
    bands <- c("PET_500m")
    scale_val <- TRUE
    output_res <- "500"
    categ <- "Ecosystem Variables - Evapotranspiration"
    prod_version <-  "006"
  }
  message("Acquiring: ", prod)
  # check that site has a boundary
  boundary <- ReLTER::get_site_info(
    deimsid,
    category = "Boundaries"
  )
  if (is.null(boundary) || !inherits(boundary, "sf")) {
    stop("No boundary for requested DEIMS site.")
  }
  # Reproject to target CRS, then get bbox
  bndry <- sf::st_transform(boundary, as.numeric(output_proj))
  bbox = sf::st_bbox(bndry)
  # Make sure we have login credentials for EarthData
  user <- Sys.getenv("earthdata_user")
  pass <- Sys.getenv("earthdata_pass")
  if (user == "" | is.null(user) | pass == "" | is.null(pass)) {
    warning("Please register at: https://urs.earthdata.nasa.gov/home",
    "then set `earthdata_user` and `earhtdata_pass`  environment variables.",
    "i.e. Sys.setenv(...)")
    stop("No login credentials for EarthData.")
  }
  if (is.null(output_dir) | output_dir==""){
    # All download rasters will be saved to tempdir()
    # and they will be removed when R exits!
    warning("No `output_dir` specified. Downloaded rasters will be saved
            to a temporary directory, and will be removed when R exits!")
    output_dir <- tempdir()
  }
  if (!dir.exists(output_dir)) {
    tryCatch(dir.create(output_dir, recursive=TRUE),
            error=function(e) {stop("Cannot create: ", output_dir, e)})
  }
  # Read in a standard json config file for MODIStsp
  # TODO: remove "inst"
  cfg <- system.file("extdata/modistsp_options.json",
                     package="ReLTER")
  # Which downloader
  dldr <- "html"
  # if (Sys.info()["sysname"] == "windows") {
  #   dldr <- "html"
  # } else {
  #   dldr <- system("which aria2c", intern = TRUE)
  #   if (is.null(dldr) | dldr=="") {
  #     dldr <- "html"
  #   } else {dldr <- "aria2"}
  # }
  
  # All set, get the requested rasters
  t0 <- Sys.time()
  message(t0, " - Download starting.")
  MODIStsp::MODIStsp(gui = FALSE,
                     opts_file = cfg,
                     out_folder = output_dir,
                     out_folder_mod = tempdir(),
                     #spafile = boundary, # We are using bbox
                     selprod = prod,
                     selcat = categ,
                     bandsel = bands,
                     spameth = "bbox",
                     bbox = bbox,
                     user = user,
                     password = pass,
                     start_date = from_date,
                     end_date = to_date,
                     download_range = download_range,
                     # sensor = "Aqua",  # "Both" set in json options
                     # output_proj = "3035",  # set in json options file
                     out_res = output_res,
                     output_proj = as.character(output_proj),
                     downloader = dldr, # "html" or "aria2" if it is installed
                     scale_val = scale_val,
                     prod_version = prod_version,
                     verbose = FALSE
  )
  
  t1 <- Sys.time()
  message(t1, " - Download completed.")
  elapsed <- difftime(t1, t0, units="mins")
  message("Elapsed time: ", sprintf("%.1f", elapsed), " minutes")
  dl_list <- list.files(output_dir, pattern=".tif$", recursive=TRUE)
  cnt_dl <- length(dl_list)
  message("Downloaded: ", cnt_dl, " MODIS files")
  
  # Additional functionality:
  # Time series line plots
  if (plot_ts) {
    plot_timeseries(deimsid,
                            product = product,
                            output_dir = output_dir,
                            output_proj=output_proj)
  }
  
  # Aggregated raster of all images in time series
  if (!show_map == FALSE) {
    if (show_map %in% c("mean", "max", "min")){
    # We nned the site name for plotting agg map
    site_name <- str_replace_all(boundary$title, "[^[:alnum:]]", "_")
    site_name <- str_replace_all(site_name, "_+", "_")
      
    plot_agg_map(product=product,
                          output_dir=output_dir,
                          site_name=site_name,
                          agg_function=show_map)
    }
  }
  return(dl_list)
}


#' Plot a time series of averaged pixel values from MODIS images.
#' 
#' @description Create a time series of averaged pixel values from MODIS images
#' cropped to site boundaries. Display a line plot and save to png.
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' 
#' @param deimsid  A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @param product A `character`. The requested product. One of:
#' "LST", VI". Default is "VI".
#' @param output_dir a `character`, where MODIS images were saved
#' This directory is returned by `get_site_MODIS()`
#' The final graph as png image file will be saved here also.
#' @param output_proj `character`: The EPSG code of output rasters

#' @details 
#' Read all images in `output_dir` and prepare line plots
#' of average pixel values over the site boundary for each band.
#' 
#' This function is not exported. It is called by `get_site_MODIS()`
#' 
#' @return Full path to the saved png image.
#' 
#' @author Micha Silver, phD (2020) \email{silverm@@post.bgu.ac.il}
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @references
#'   \insertRef{sfR}{ReLTER}
#'
#'   \insertRef{terraR}{ReLTER}
#' @examples
#'  \dontrun{
#'  # Example in Northern Negev LTER
#' deimsid <- "https://deims.org/871a90b2-e372-456a-93e3-518ad1e11239"
#' output_dir <- tempdir()
#' png_files <- ReLTER::plot_timeseries(deimsid, product="VI",
#'     output_dir=output_dir, output_proj="6991")
#' message("Output plots: ", png_files)
#' }
#' @keywords Internal
#' 
plot_timeseries = function(deimsid, product,
                           output_dir, output_proj="3035") {
  boundary <- ReLTER::get_site_info(
    deimsid,
    category = "Boundaries"
  )
  if (is.null(boundary) || !inherits(boundary, "sf")) {
    stop("No boundary for requested DEIMS site.")
    return(NULL)
  }
  # Reproject and Convert boundary to terra vect
  bndry <- sf::st_transform(boundary, as.numeric(output_proj))
  bndry <- terra::vect(bndry)
  site_name <- str_replace_all(boundary$title, "[^[:alnum:]]", "_")
  site_name <- str_replace_all(site_name, "_+", "_")
  
  # Get subdirs for each product
  # First load the full set of product options from MODIStsp
  opts_file <- system.file("ExtData/MODIStsp_ProdOpts.RData",
                           package="MODIStsp")
  load(opts_file)  # Now we have the list 'prod_opt_list'
  
  if (product == "VI") {
    prod <- 'Vegetation Indexes_16Days_250m (M*D13Q1)'
    vers='061'
  } else if (product == "LST") {
    prod <- "LST_3band_emissivity_8day_1km (M*D21A2)"
    vers='061'
  } else if (product == "LAI") {
    prod <- "LAI_8Days_500m (M*D15A2H)"
    vers='061'
  } else {  # "ET"
    prod <- "Net_ET_8Day_500m (M*D16A2)"
    vers='006'
  }
  out_subdir <- prod_opt_list[[prod]][[vers]]$main_out_folder
  # Get directory of time series VRT files
  prod_dir <- file.path(output_dir, out_subdir)
  # and list of subdirs of each band
  dir_list <- list.dirs(prod_dir,
                        full.names = TRUE,
                        recursive=FALSE) # avoid the subdirs
  # Ignore the Time_Series subdir
  dir_list <- dir_list[! grepl("Time_Series", dir_list)]
  # Now we are left with only the band subdirs
  # Loop over band directories and create a time series plot for each
  pths <- lapply(1:length(dir_list), function(dl) {
    
    # Read all *.tif into terra::rast stack
    band_dir <- dir_list[dl]
    #print(paste(dl, band_dir))
    tif_list <- list.files(band_dir,
                           pattern = ".tif$", full.names = TRUE)
    #print(tif_list[90])

    if (length(tif_list) == 0) {
      stop("No timeseries files in: ", band_dir)
    } else {
      stk <- terra::rast(tif_list)
      # The MODIStsp rasters are set with PROJCRS='unknown', so set CRS
      terra::crs(stk) <- paste0("EPSG:", output_proj)
      # Get dates from file names
      date_list <- lapply(tif_list, function(f){
        # Underscore is MODIStsp default separator
        dte = gsub(pattern=".tif", replacement="", x=basename(f))
        parts <- unlist(strsplit(dte, split = "_")) 
        # Get real date
        dte <- as.Date(paste(parts[length(parts)-1],
                             parts[length(parts)]), format="%Y %j")
        # Convert back to string
        dte <- data.frame("Date" = dte)
        return(dte)
      })
      dates_df = do.call(rbind, date_list)
      
      # Extract mean of all raster bands. Use na.rm=TRUE to drop NA pixels
      site_vals <- terra::extract(stk, bndry, fun=mean,
                                  ID=FALSE, exact=TRUE, bind=FALSE,
                                  raw=FALSE, na.rm=TRUE)
      # Transform layers to columns
      site_vals <- t(site_vals)
      # Join dates
      site_df <- cbind(site_vals, dates_df, deparse.level = 0)
      names(site_df) <- c("Value", "Date")
      # Be sure to remove NA rows
      site_df <- site_df[complete.cases(site_df),]
      #message("Site DataFrame: ", length(site_df$Value))
      
      # Now plot
      ttl <- paste0(site_name, ": ", basename(band_dir))
      pl <- ggplot2::ggplot(data = site_df) +
        ggplot2::geom_line(aes(x=Date, y=Value, group = 1),
                           color="blue", size = 0.8, alpha=0.7 ) + 
        ggplot2::geom_point(aes(x=Date, y=Value),
                            color="blue", size = 2.0, alpha=0.3 ) + 
        #scale_color_manual(values = clrs) +
        ggplot2::ggtitle(ttl) +
        ggplot2::theme(axis.text = element_text(size=10),
              axis.title = element_text(size=10),
              axis.text.x = element_text(angle = 30, size=8,
                                         hjust=1, vjust=1),
              title = element_text(size=12, face="bold"))
      
      print(pl)
      # and save plot to png
      png_file = paste(site_name, basename(band_dir),
                       "timeseries.png", sep="_")
      png_path = file.path(prod_dir, png_file)
      png(png_path, width=800)
      print(pl)
      dev.off()
      #ggplot2::ggsave(filename = png_path,plot = pl,
      #               width=8, height=5)
      return(png_path)
    } # end if(length(tif_list...))
  }) # end lapply
  message("Paths to time series plots:")
  print(unlist(pths))
  return(pths)
}


#' Map of aggregated time series of MODIS images
#' 
#' @description Prepare, show and save an aggregated map
#' of acquired MODIS products
#' 
#' @param product `character` one of "LST" or "VI"
#' @param output_dir `character`, where MODIS images were saved
#' This directory is returned by `get_site_MODIS()`
#' The final map, as png image file will be saved here also.
#' @param site_name `character` the site (passed from get_site_MODIS())
#' @param agg_function `character` either FALSE (the default) or one of
#' "mean", "max", "min". All maps in time series will be aggregated
#' using this function.
#' 
#' @details Read all time series images (from *.vrt file) in `output_dir` 
#' Prepare an aggregation raster of all maps in the time series
#' Save and show a plot of the aggregated map
#' 
#' This function is not exported. It is called by `get_site_MODIS()`
#' @return Full paths to saved Geotiff rasters
#' 
#' @author Micha Silver, phD (2020) \email{silverm@@post.bgu.ac.il}
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @references
#'   \insertRef{sfR}{ReLTER}
#'
#'   \insertRef{terraR}{ReLTER}
#' @keywords Internal

plot_agg_map = function(product, output_dir,
                        site_name, agg_function="mean") {
  # Use parameters to find full path to output subdir
  opts_file <- system.file("ExtData/MODIStsp_ProdOpts.RData",
                           package="MODIStsp")
  load(opts_file)  # Now we have the list 'prod_opt_list'
  vers='061'
  if (product == "VI") {
    prod <- 'Vegetation Indexes_16Days_250m (M*D13Q1)'
  } else if (product == "LST") { # LST
    prod <- "LST_3band_emissivity_8day_1km (M*D21A2)"
  } else if (product == "LAI") {
    prod <- "LAI_8Days_500m (M*D15A2H)"
  } else {  # "ET"
    prod <- "Net_ET_8Day_500m (M*D16A2)"
    vers <- "006"
  }
  out_subdir <- prod_opt_list[[prod]][[vers]]$main_out_folder
  # Use "Time_Series" subdir, with Mixed Aqua and Terra 
  prod_dir <- file.path(output_dir, out_subdir)
  vrt_subdir <- file.path(prod_dir,
                          "Time_Series", "GDAL", "Mixed")
  vrt_list <- list.files(vrt_subdir, pattern=".vrt$",
                        full.names = TRUE, recursive = TRUE)
  if (length(vrt_list)==0) {
    # Something is wrong with time series
    stop("No time series data were found.")
    return(NULL)
  }

  pths <- lapply(vrt_list, function(v){
    pth_parts <- unlist(strsplit(v, "/"))
    prod <- pth_parts[length(pth_parts)-1]
    r <- terra::rast(v)
    r_agg <- terra::app(r, agg_function)
    tif_name <- paste(site_name, prod, "aggregated.tif", sep="_")
    tif_path <- file.path(output_dir, out_subdir, tif_name)
    terra::writeRaster(r_agg, tif_path, overwrite=TRUE)
    if (packageVersion("leaflet") > "2.1.1") {
      print(terra::plet(r_agg,
                        main = paste0(site_name, ": ", prod),
                        tiles = "Streets"))
    }
    return(tif_path)
  })
  message("Paths to aggregated maps:")
  print(unlist(pths))
  return(pths)
}
