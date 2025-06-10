#' eLTER reporting format naming convention for files
#' @description `r lifecycle::badge("experimental")`
#' Compose file name following eLTER naming convention
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @param deimsid A `character` The DEIMS ID of the site from
#' DEIMS-SDR website. More information about DEIMS ID in this pages:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @param data_topic A `character`. Max 5-digit code for data topic or
#' observation programme, e.g. METEO (Meteorology), BIODIV (Biodiversity),
#' DEPO (deposition), GHG (Green House gas), SW (Soil water), VEG (Vegetation).
#' The abbreviation is defined by the data provider depending on the data.
#' @param variable_group A `character`. Optional, list of variables or
#' variable groups contained in the data. The abbreviation is defined by the
#' data provider depending on the data.
#' @param time_span A `numeric` or a `character`. Time span covered in the
#' data. E.g. 2015, 20150302-20180415. The time span is defined by the data
#' provider.
#' @param version version in format "VYYYYMMDD". Data version in the format
#' “V”YYYYMMDD.
#' Defaults to current system date.
#' @return filename (without extension) following naming convention
#' @importFrom stringr str_replace_all
#' @importFrom dplyr pull
#' @references
#'   \insertRef{stringrR}{ReLTER}
#' 
#'   \insertRef{dplyrR}{ReLTER}
#' @seealso Peterseil, Geiger et al. (2020)
#' Field Specification for data reporting. Technical Document.
#' TechDoc.01. EU Horizon 2020 eLTER PLUS Project, Grant agreement No. 871128
#' \url{https://zenodo.org/record/6373410}
#' @note This method must be intended as a signpost for future implementation
#' @keywords internal
#' @examples
#' \dontrun{
#' ## Not run:
#' 
#' deimsid <- "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6"
#' time_span <- 2015 # e.g. whole year
#' # time_span <- "20150302-20180415" # e.g. span between two dates
#' data_topic <- "VEG" # data provider defined abbreviation of "vegetation"
#' variable_group <- "SPECCOVER" # data provider defined abbreviation
#' version <- "V20220907"
#'
#' filename <- reporting_compose_file_name(
#'   deimsid = deimsid,
#'   data_topic = data_topic,
#'   variable_group = variable_group,
#'   time_span = time_span,
#'   version = version
#' )
#' 
#' ## End (Not run)
#' }
#'
### function reporting_compose_file_name
reporting_compose_file_name <- function(
  deimsid = NULL,
  data_topic,
  variable_group = "",
  time_span,
  version = Sys.Date() %>% format("V%Y%m%d")
) {

  if (!is.null(deimsid)) {
    info <- get_site_info(deimsid)
    country_code <- isoCodes$Alpha_3[isoCodes$Name == info$data$country]
    site_name <- stringr::str_replace_all(info$data$title, " ", replacement = "-")
  } else if(is.null(country_code) || is.null(site_name)) 
    stop("if deimsid is not specified, both country_code and site_name are required")
  return(
    paste(
      country_code, site_name,
      data_topic, variable_group,
      time_span, version,
      sep = "_"
    )
  )
}

#' Compose an eLTER Data Reporting Format object
#' @description `r lifecycle::badge("experimental")`
#' Given several tables, creates an eLTER data reporting format object
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @param station A `tibble` containing the station information. Station is an
#' observation entity within a site or platform. Station in this respect is
#' synonym to plot, observation location, sensor location, etc. and is defined
#' by a location, elevation and installation height (if relevant) which is
#' located within a given LTER site or platform.
#' @param method A `tibble` containing the method information. Method describes
#' the procedure to generate and manipulate the data.
#' @param data A `tibble` containing the data. Data are defined as the sum of
#' observation values being observed at a station/plot either by sensor,
#' measurement device or human observation.
#' @param reference A `tibble` containing the reference information. Reference
#' is the listing and description of additional codes used in the data
#' provision.
#' @param event A `tibble` containing the event information. Event is defined
#' as activity to observe or collect information on the ecosystem
#' characteristic of interest.
#' @param sample_event A `tibble` containing the sample event information.
#' Sample event is the key observational units in environmental sciences, i.e.
#' ecology, geosciences, biogeochemistry, and hydrobiology, and are essential
#' to document and further analyse biological communities in laboratories
#' (e.g., phytoplankton communities in water samples, benthic communities in
#' sediments, etc.).
#' @param license A `character`. It is a textual description of the conditions
#' to use for the data.
#' @inheritParams reporting_compose_file_name
#' @seealso Peterseil, Geiger et al. (2020)
#' Field Specification for data reporting. Technical Document.
#' TechDoc.01. EU Horizon 2020 eLTER PLUS Project, Grant agreement No. 871128
#' \url{https://zenodo.org/record/6373410}
#' @return list with eLTER reporting format slots
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#' deimsid <- "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6"
#' time_span <- 2015 # e.g. whole year
#' # time_span <- "20150302-20180415" # e.g. span between two dates
#' data_topic <- "VEG" # data provider defined abbreviation of "vegetation"
#' variable_group <- "SPECCOVER" # data provider defined abbreviation
#' version <- "V20220907"
#'
#' data <- tibble::tribble(
#'                                                    ~SITE_CODE,   ~VARIABLE,        ~TIME,   ~VALUE,    ~UNIT,
#'      "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",      "TEMP", "2016-03-15",    "5.5",     "°C",
#'      "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",      "PREC", "2016-03-03",   "10.2",     "mm",
#'      "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",      "TEMP", "2016-02-15",    "2.5",     "°C",
#'      "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",      "NH4N",    "2016-03",    "5.5",   "mg/l",
#'      "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",      "SO4S",    "2016-03",   "10.2",   "mg/l",
#'      "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",        "CA",    "2016-03",    "2.5",   "mg/l"
#' )
#' station <- dplyr::tribble(
#'   ~SITE_CODE, ~STATION_CODE, ~STYPE, ~LAT,      ~LON,       ~ALTITUDE,
#'   deimsid,    "IP2",         "AREA",  45.340805, 7.88887495, 265
#' )
#' method <- dplyr::tribble(
#'   ~VARIABLE, ~METH_DESCR,
#'   "COVE_F",  "Analysis of ammonium..." 
#' )
#' 
#' research_object <- reporting_produce_data_object_v2.0(
#'   station = station,
#'   method = method,
#'   data = data,
#'   deimsid = deimsid,
#'   data_topic = data_topic,
#'   variable_group = variable_group,
#'   time_span = time_span,
#'   version = version
#' )
#' 
#' }
#' ## End (Not run)
#'
### function reporting_produce_data_object_v2.0
reporting_produce_data_object_v2.0 <- function(
    station = NULL,
    method = NULL,
    data = NULL,
    reference = NULL,
    event = NULL,
    sample_event = NULL,
    license = NULL,
    deimsid = NULL,
    data_topic,
    variable_group = "",
    time_span,
    version = Sys.Date() %>% format("V%Y%m%d")
) {
  # filename
  filename <- reporting_compose_file_name(
    deimsid = deimsid,
    data_topic = data_topic,
    variable_group = variable_group,
    time_span = time_span,
    version = version
  )
  # output
  return(list(
    filename = filename,
    deimsid = deimsid,
    data_topic = data_topic,
    variable_group = variable_group,
    time_span = time_span,
    version = version,
    STATION = station,
    METHOD = method,
    DATA = data,
    REFERENCE = reference,
    EVENT = event,
    SAMPLE_EVENT = sample_event,
    LICENSE = license
  ))
}

#' Creates an archive with files following the eLTER reportingFormat
#' @description `r lifecycle::badge("experimental")`
#' Creates a zip archive "filename".zip
#' @param x A `list` like the one created by function
#' `reporting_produce_data_object_v2.0`
#' @param filepath A `character` file path. Defaults to temporary directory
#' @param saveRDS A `logical`. Save also object in RDS format.
#' Defaults to FALSE
#' @return named A `list` containing paths to saved files filepaths.
#' Slots are named "zip" and possibly "RDS". A path to the zip file, or rds
#' file, can be defined by the parameter `filepath`.
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @importFrom utils zip write.csv2
#' @note This method must be intended as a signpost for future implementation
#' @seealso Peterseil, Geiger et al. (2020)
#' Field Specification for data reporting. Technical Document.
#' TechDoc.01. EU Horizon 2020 eLTER PLUS Project, Grant agreement No. 871128
#' \url{https://zenodo.org/record/6373410}
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#'
#' archive <- reporting_save_archive(
#'   x = research_object,
#'   # obtained from the function `reporting_produce_data_object_v2.0()`
#'   filepath = ".",
#'   saveRDS = TRUE
#' )
#' 
#' ## End (Not run)
#' }
#'
### function reporting_save_archive
reporting_save_archive <- function(
  x,
  filepath = tempdir(),
  saveRDS = FALSE
) {
  sr <- x$filename
  deimsid <- x$deimsid
  dirsr <- paste0(filepath, "/", sr)
  lx <- sapply(x, function(y) "tbl_df" %in% class(y))
  slotstosave <- names(lx)[lx]
  filenames <- paste0(dirsr, "/", slotstosave, ".csv")
  # clean up existing previous work
  if (dir.exists(dirsr)) {
    file.remove(paste0(dirsr, "/", list.files(dirsr)))
    file.remove(dirsr)
  }
  dir.create(dirsr)
  lapply(slotstosave, FUN = function(y) {
    fname <- paste0(dirsr, "/", y, ".csv")
    utils::write.csv2(x[y], fname)
  })
  utils::zip(
    paste0(filepath, "/", x$filename, ".zip"),
    files = dirsr,
    extras = "-j"
  )
  savedFiles <- list()
  savedFiles["zip"] <- paste0(filepath, "/", x$filename, ".zip")
  if (saveRDS == TRUE) {
    saveRDS(x, file = paste0(filepath, "/", x$filename, ".RDS"))
    savedFiles["RDS"] <- paste0(filepath, "/", x$filename, ".RDS")
  }
  # output
  return(savedFiles)
}

#' Compose an eLTER Data Reporting Format object
#' @description `r lifecycle::badge("deprecated")`
#' Given several tables, creates an eLTER data reporting format
#' object
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @param data A `tibble`. See eLTER data specification format for details
#' @param station A `tibble`
#' @param method A `tibble`
#' @param reference A `tibble`
#' @param event A `tibble`
#' @param sample A `tibble`
#' @param license A `character`
#' @param deimsid A character. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @param data_type A `character`. Data must be provided by one of measurement
#' or mapping.
#' Default 'measurement'
# @param data_orientation A `character`. Data must be provided in to ways by
# row or by column. Indicate 'row' if each observation, defined as the
# combination of location, time, variable and value, is organised in a single
# row. Indicate 'column' if each observation is organised as spreadsheet with
# location and time in rows, variables in column and value as cell entry. The
# method, the unit as well as additional information (like quality flags) for
# the variable needs to be defined in the METHOD if possible.
# Default 'row'.
#' @param filename optional filename associated with the object, of the form
#' provided as output by the function `reporting_compose_file_name`
#' @seealso Peterseil, Geiger et al. (2020)
#' Field Specification for data reporting. Technical Document.
#' TechDoc.01. EU Horizon 2020 eLTER PLUS Project, Grant agreement No. 871128
#' \url{https://zenodo.org/record/6373410}
#' @return list with eLTER reporting format slots
#' @note This method must be intended as a signpost for future implementation
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#' 
#' deimsid <- "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6"
#' time_span <- 2015 # e.g. whole year
#' # time_span <- "20150302-20180415" # e.g. span between two dates
#' data_topic <- "VEG" # data provider defined abbreviation of "vegetation"
#' variable_group <- "SPECCOVER" # data provider defined abbreviation
#' version <- "V20220907"
#'
#' filename <- reporting_compose_file_name(
#'   deimsid = deimsid,
#'   data_topic = data_topic,
#'   variable_group = variable_group,
#'   time_span = time_span,
#'   version = version
#' )
#'
#' data <- tibble::tribble(
#'                                                    ~SITE_CODE,   ~VARIABLE,        ~TIME,   ~VALUE,    ~UNIT,
#'      "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",      "TEMP", "2016-03-15",    "5.5",     "°C",
#'      "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",      "PREC", "2016-03-03",   "10.2",     "mm",
#'      "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",      "TEMP", "2016-02-15",    "2.5",     "°C",
#'      "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",      "NH4N",    "2016-03",    "5.5",   "mg/l",
#'      "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",      "SO4S",    "2016-03",   "10.2",   "mg/l",
#'      "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",        "CA",    "2016-03",    "2.5",   "mg/l"
#' )
#' station <- dplyr::tribble(
#'   ~SITE_CODE, ~STATION_CODE, ~STYPE, ~LAT,      ~LON,       ~ALTITUDE,
#'   deimsid,    "IP2",         "AREA",  45.340805, 7.88887495, 265
#' )
#' method <- dplyr::tribble(
#'   ~VARIABLE, ~METH_DESCR,
#'   "COVE_F",  "Analysis of ammonium..." 
#' )
#' 
#' research_object <- reporting_produce_data_object_v1.3(
#'  filename = filename,
#'  deimsid = deimsid,
#'  data = data,
#'  station = station,
#'  method = method
#' )
#' 
#' }
#' ## End (Not run)
#'
### function reporting_produce_data_object_v1.3
reporting_produce_data_object_v1.3 <- function(data = NULL, station = NULL,
                                               method = NULL, reference = NULL,
                                               event = NULL, sample = NULL,
                                               license = "", deimsid = "",
                                               data_type = "measurement",
                                               #data_orientation = "row",
                                               filename = NULL) {
  if (!data_type %in% c("measurement", "mapping"))
    stop("data type must be one of measurement or mapping")
  # if (!data_orientation %in% c("row", "column"))
  #   stop("data orientation must be one of row or column")
  return(list(
    filename = filename,
    type = data_type,
    deimsid = deimsid,
    DATA = data,
    STATION = station,
    METHOD = method,
    REFERENCE = reference,
    EVENT = event,
    SAMPLE = sample,
    LICENSE = license
  ))
}
