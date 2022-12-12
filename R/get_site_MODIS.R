#' Acquire a time series either of two datasets from MODIS satellites:
#' Land Surface Temperature (LST) or Vegetation Index (NDVI)
#' 
#' cropped to an eLTER site boundary.
#' @description `r lifecycle::badge("stable")`
#' Download a timeseries of MODIS images containing the requested
#' dataset and optionally plot a time series graph
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
#' @param dataset A `character`. The requested dataset. One of:
#' "LST_day", "LST_night", VI"
#' Default is "VI".
#' @param from_date a `character`, the start date formatted as YYYY.MM.DD 
#' @param to_data a `character`, the end date formatted as YYYY.MM.DD
#' @param output_dir a `character`, where to save downloaded rasters
#' @param plot_ts a `boolean`, whether to plot the time series, 
#'    default TRUE
#'    
#' @details Certain layers from one of these MODIS products are acquired.
#' from:
#'    "LST_3band_emissivity_Daily_1km (M*D21A1D)" and
#'    "LST_3band_emissivity_Daily_1km_night (M*D21A1N)"
#' the "Land surface temperature" band is acquired.
#' from:
#'     "Vegetation Indexes_16Days_250m (M*D13Q1)"
#' two bands are acquired: "16 day NDVI average" and "16 day EVI average"
#' NOTE that the default `output_dir` is tempdir(), so the downloaded files
#' will be deleted when exiting R.
#' 
#' @return Full path of all downloaded Geotiff files,
#' cropped to the site boundaries.
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
#' deimsid <- "https://deims.org/871a90b2-e372-456a-93e3-518ad1e11239"
#' from_date <- "2018.01.01"
#' to_date <- "2019.12.31"
#' output_dir <- "/home/micha/work/tmp/MODIS"
#' ReLTER::get_site_MODIS(deimsid, dataset="VI",
#'     from_date=from_date, to_date=to_date, output_dir=output_dir)
#' }
#'
### function get_site_MODIS
get_site_MODIS <- function(deimsid, dataset = "VI",
                         from_date='2010.01.01', to_date='2020.31.12',
                         output_dir=NULL,
                         plot_ts=TRUE) {
  prod <- dplyr::case_when(
    dataset == "VI" ~ "M*D13Q1",
    dataset == "LST_day" ~ "M*D21A1D",
    dataset == "LST_night" ~ "M*D21A1N",
    TRUE ~ paste("Dataset:", dataset, "unavailable")
  )
  # Make sure the requested product is among those supported
  if (grepl(pattern = "unavailable", x = prod, fixed = TRUE)) {
    stop(paste(prod, "not supported"))
    return(NULL)
  }
  if (dataset == "VI") {
    bands <- c("NDVI", "EVI")
    scale_val <- TRUE
    output_res  <-  250
  } else {
    bands <- "LST_1KM"
    scale_val <- FALSE
    output_res = 1000
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
  bbox = sf::st_bbox(boundary)
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
  cfg <- file.path("extdata", "modistsp_options.json")
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
                     bandsel = bands,
                     spameth = "bbox",
                     bbox = bbox,
                     user = user,
                     password = pass,
                     start_date = from_date,
                     end_date = to_date,
                     # sensor = "Aqua",  # "Both" set in json options
                     # output_proj = 3035,  # set in json options file
                     output_res = output_res,
                     downloader = dldr, # "html" or "aria2" if it is installed
                     scale_val = scale_val,
                     verbose = FALSE
  )
  t1 <- Sys.time()
  message(t1, " - Download completed.")
  elapsed <- difftime(t1, t0, "mins")
  message("Elapsed time: ", sprintf("%.2f", elapsed), "minutes")
  dl_list <- list.files(output_dir, pattern=".tif$", recursive=TRUE)
  cnt_dl <- length(dl_list)
  message("Downloaded: ", cnt_dl, " MODIS files")
  if (plot_ts) {
    plot_png <- ReLTER::plot_timeseries(deimsid,
                                        dataset, output_dir)
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
#' @param dataset A `character`. The requested dataset. One of:
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
#' 
#' }
#'
plot_timeseries = function(deimsid, dataset, output_dir) {
  boundary <- ReLTER::get_site_info(
    deimsid,
    category = "Boundaries"
  )
  if (is.null(boundary) || !inherits(boundary, "sf")) {
    stop("No boundary for requested DEIMS site.")
    return(NULL)
  }
  site_name <- str_replace_all(boundary$title, "[^[:alnum:]]", "_")
  site_name <- str_replace_all(site_name, "_+", "_")
  
  # Get subdirs for each product
  dir_list <- list.dirs(output_dir, full.names = TRUE, recursive = TRUE)
  # Read all *.tif into terra::rast stack
  tif_list <- list.files(output_dir, pattern = ".tif$", full.names = TRUE)
  stk <- terra::rast(tif_list)
  # Convert boundary to terra vect
  bndry <- terra::vect(boundary)
  mean_values <- terra::extract(stk, bndry, fun=mean,
                                ID=FALSE, exact=TRUE, bind=FALSE, raw=FALSE)

  pl = ggplot2::ggplot(data = mean_values) +
    ggplot2::geom_line(aes(x=Date, y=Value, color = Statistic),
              size = 0.6, alpha=0.7 ) + 
    ggplot2::geom_point(aes(x=Date, y=Value, color = Statistic),
               size = 0.6, alpha=0.5 ) + 
    #scale_color_manual(values = clrs) +
    ggplot2::ggtitle(paste(site, prod)) +
    ggplot2::theme(axis.text = element_text(size=10),
          axis.title = element_text(size=12),
          title = element_text(size=12, face="bold"))
  #print(pl)

  # save merged plot
  png_file = paste(site_name, dataset, "timeseries.png", sep="_")
  png_path = file.path(output_dir, png_file)
  ggplot2::ggsave(png_path)
  return(png_path)
}
