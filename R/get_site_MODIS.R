#' Acquire a time series either of two products from MODIS satellites:
#' Land Surface Temperature (LST) or Vegetation Index (NDVI)
#' 
#' cropped to an eLTER site boundary.
#' @description `r lifecycle::badge("stable")`
#' Download a timeseries of MODIS images containing the requested
#' product and optionally plot a time series graph
#' of the average values over the site.
#' Use of this function requires registering on the EarthData website:
#' Requires registration on EarthData website:
#'    https://urs.earthdata.nasa.gov/home
#' In order to guard your user credentials, please save
#' your username and password to environment variables. i.e.
#'  Sys.setenv(earthdata_user="homer_simpson")
#'  Sys.setenv(earthdata_pass="bart&lucy")
#' 
#' @param deimsid  A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @param product A `character`. The requested product. One of:
#' "LST" or "VI". Default is "VI".
#' @param from_date a `character`, the start date formatted as YYYY.MM.DD 
#' @param to_data a `character`, the end date formatted as YYYY.MM.DD
#' @param output_dir a `character`, where to save downloaded rasters
#' @param plot_ts a `boolean`, whether to plot the time series, 
#'    default TRUE
#' @param output_proj a `character`, the EPSG code of desired output projection
#' default is "3035", the European LAEA coordinate reference system.    
#' @details Certain layers from one of these MODIS products are acquired.
#' from:
#'    "LST_3band_emissivity_8day_1km (M*D21A2)" )"
#'     two "Land surface temperature" bands are acquired:
#'     "LST_Day_1KM", "LST_Night_1KM"
#' from:
#'     "Vegetation Indexes_16Days_250m (M*D13Q1)"
#'     two VI bands are acquired:
#'     "NDVI" and "EVI"
#' NOTE that the default `output_dir` is tempdir(), so the downloaded
#' MODIS files will be deleted when exiting R.
#' Enter a path for `output_dir` to save the files.
#' 
#' @return Full path of all downloaded Geotiff files,
#' cropped to the site boundaries.
#' 
#' @author Micha Silver, phD (2020) \email{silverm@@post.bgu.ac.il}
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @references
#'   \insertRef{sfR}{terraR}{ReLTER}
#'
#' @export
#' @examples
#'  \dontrun{
#' # Example in Northern Negev LTER
#' deimsid <- "https://deims.org/871a90b2-e372-456a-93e3-518ad1e11239"
#' from_date <- "2018.01.01"
#' to_date <- "2018.04.30"
#' output_dir <- tempdir()
#' ReLTER::get_site_MODIS(deimsid, product="VI",
#'     from_date=from_date, to_date=to_date,
#'     output_dir=output_dir, plot_ts=FALSE, output_proj="6991")
#' }
#'
### function get_site_MODIS
get_site_MODIS <- function(deimsid, product = "VI",
                         from_date='2010.01.01', to_date='2020.31.12',
                         output_dir=NULL,
                         plot_ts=TRUE,
                         output_proj = "3035") {

  # Make sure the requested product is among those supported
  if (! product %in% c("VI", "LST")) {
    stop(paste(product, "not supported"))
    return(NULL)
  }
  # Setup category and product for the chosen product
  if (product == "VI") {
    prod <- 'Vegetation Indexes_16Days_250m (M*D13Q1)'
    categ <- "Ecosystem Variables - Vegetation Indices"
    bands <- c("NDVI", "EVI")
    scale_val <- TRUE
    output_res  <-  "250"
  } else { # LST
    prod <- "LST_3band_emissivity_8day_1km (M*D21A2)"
    categ <- "Radiation Budget Variables - Land Surface Temperature/Emissivity"
    bands <- c("LST_Day_1KM", "LST_Night_1KM")
    scale_val <- TRUE
    output_res = "1000"
  }
  # check that site has a boundary
  boundary <- ReLTER::get_site_info(
    deimsid,
    category = "Boundaries"
  )
  if (is.null(boundary) || !inherits(boundary, "sf")) {
    stop("No boundary for requested DEIMS site.")
    return(NULL)
  }
  # Reproject to target CRS, then get bbox
  bndry <- sf::st_transform(boundary, as.numeric(output_proj))
  bbox = sf::st_bbox(bndry)
  # Make sure we have login credentials for EarthData
  user <- Sys.getenv("earthdata_user")
  pass <- Sys.getenv("earthdata_pass")
  if (user == "" | is.null(user) | pass == "" | is.null(pass)) {
    warning("Please register at: https://urs.earthdata.nasa.gov/home")
    warning("then set `earthdata_user` and `earhtdata_pass`
            environment variables. i.e. Sys.setenv(...)")
    stop("No login credentials for EarthData.")
    return(NULL)
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
  dldr <- system("which aria2c", intern = TRUE)
  if (is.null(dldr) | dldr=="") {
    dldr <- "html"
  } else {dldr <- "aria2"}
  
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
                     # sensor = "Aqua",  # "Both" set in json options
                     # output_proj = "3035",  # set in json options file
                     out_res = output_res,
                     output_proj = as.character(output_proj),
                     downloader = dldr, # "html" or "aria2" if it is installed
                     scale_val = scale_val,
                     verbose = FALSE
  )
  t1 <- Sys.time()
  message(t1, " - Download completed.")
  elapsed <- difftime(t1, t0, units="mins")
  message("Elapsed time: ", sprintf("%.1f", elapsed), " minutes")
  dl_list <- list.files(output_dir, pattern=".tif$", recursive=TRUE)
  cnt_dl <- length(dl_list)
  message("Downloaded: ", cnt_dl, " MODIS files")
  if (plot_ts) {
    plot_png <- ReLTER::plot_timeseries(deimsid,
                                        product = product,
                                        output_dir = output_dir,
                                        output_proj=output_proj)
    message("Timeseries plot saved to", plot_png)
  }
  return(dl_list)
}


#' Plot a time series of average pixel values from MODIS images
#' cropped to an eLTER site boundary.
#' 
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' #' @param deimsid  A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @param product A `character`. The requested product. One of:
#' "LST_day", "LST_night", VI"
#' Default is "VI".
#' @param output_dir a `character`, where MODIS images were saved
#' This directory is returned by `get_site_MODIS()`
#' The final graph as png image file will be saved here also.

#' @details Read all images in `output_dir` and prepare line plots
#' of average values over the site boundary for each band.
#' 
#' @return Full path to the saved png image.
#' 
#' @author Micha Silver, phD (2020) \email{silverm@@post.bgu.ac.il}
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @references
#'   \insertRef{sfR}{ReLTER}
#'
#'   \insertRef{terraR}{ReLTER}
#' @export
#' @examples
#'  \dontrun{
#'  # Example in Northern Negev LTER
#' deimsid <- "https://deims.org/871a90b2-e372-456a-93e3-518ad1e11239"
#' output_dir <- tempdir()
#' png_files <- ReLTER::plot_timeseries(deimsid, product="VI",
#'     output_dir=output_dir, output_proj="6991")
#' message("Output plots: ", png_files)
#' }
#'
plot_timeseries = function(deimsid, product,
                           output_dir,
                           output_proj="3035") {
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
  dir_list <- list.dirs(output_dir, full.names = TRUE, recursive = TRUE)
  # Loop over directories and create a time series plot for each
  pths <- list()
  for (d in seq_along(dir_list)) {
    # Read all *.tif into terra::rast stack
    tif_list <- list.files(dir_list[d],
                           pattern = ".tif$", full.names = TRUE)
    if (length(tif_list) == 0) {
      # This is an intermediate directory, not a leaf. Just skip
      next
    }
    stk <- terra::rast(tif_list)
    
    # Get dates from file names
    date_list <- lapply(tif_list, function(t){
      # Underscore is MODIStsp default separator
      dte = gsub(pattern=".tif", replacement="", x=basename(t))
      parts <- unlist(strsplit(dte, split = "_")) 
      # Get real date
      dte <- as.Date(paste(parts[length(parts)-1],
                           parts[length(parts)]), format="%Y %j")
      # Convert back to string
      dte <- data.frame("Date" = dte)
      # return(strptime(dte, format("%d-%m-%Y")))
      return(dte)
    })
    dates_df = do.call(rbind, date_list)
    # names(stk) = paste("Date", date_list, sep="_")

    site_vals <- terra::extract(stk, bndry, fun=mean,
                                  ID=FALSE, exact=TRUE, bind=FALSE, raw=FALSE)
    # Transform layers to to columns
    site_vals <- t(site_vals)
    # Join dates
    site_df <- cbind(site_vals, dates_df, deparse.level = 0)
    names(site_df) <- c("Value", "Date")
    
    ttl <- paste0(site_name, ": ", basename(dir_list[d]))
    pl = ggplot2::ggplot(data = site_df) +
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
  
    # save merged plot
    png_file = paste(site_name, product, "timeseries.png", sep="_")
    png_path = file.path(output_dir, png_file)
    ggplot2::ggsave(png_path, width=18, height=9, units = "cm")
    pths <- append(pths, png_path)  
  }
  # Return list of paths to png files
  return(pths)
}
