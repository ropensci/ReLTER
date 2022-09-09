#' Harmonize outputs of get_site_speciesOccurrence and map them into eLTER
#' reporting format
#' @param x A `tibble` like one that can be obtained by
#' as_tibble(get_site_speciesOccurrence(deimsid, "gbif")$gbif)
#' @param deimsid A `character`. The DEIMS.iD of the site from
#' DEIMS-SDR website. DEIMS ID information
#' @return list with the following named elements:
#' * deimsid: the same deimsid passed in input
#' * source: one of "gbif", "inat", "obis"
#' * data_mapping: tibble structured according to data_mapping of
#' eLTER reporting format
#' * reference_TAXA: tibble structured according to reference_TAXA of
#' eLTER reporting format
#' * reference_VARIABLES: tibble structured according to reference_VARIABLES
#' of eLTER reporting format
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @author Martina Zilioli \email{zilioli.m@@irea.cnr.it}
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @importFrom tibble as_tibble
#' @importFrom dplyr select mutate filter
#' @export
map_occ_gbif2elter <- function(x, deimsid) {
  # "variables" (in this case they could be defined "custom fields",
  # being just record metadata)
  # to be described in the reference.csv file
  reference_variables <- reference_variables_gbif
  custom_fields <- reference_variables$VARIABLE_CODE

  # compose output for eLTER reporting format
  # (intermediate step: info both for data and for reference)
  lter_temp <- x %>%
    .add_site_code(deimsid) %>%
    # add new (computed) columns
    dplyr::mutate(
      VARIABLE = "occurrence",
      VALUE = TRUE,
      TIME = paste0(eventDate, "T", eventTime),
      ref_CODE_URL = paste0(
        "https://www.gbif.org/species/",
        taxonKey
      )
    ) %>%
    # select and alias existing columns
    dplyr::select(
      SITE_CODE,
      ABS_POSITION = geometry,
      TAXA = taxonKey,
      VARIABLE,
      VALUE,
      TIME,
      ORG_NAME = prov,
      ref_NAME = acceptedScientificName,
      ref_CODE_URL,
      # custom fields/variables
      RECORD_ID = key, #tblGarda_gbif$institutionCode
      DATASET_KEY = datasetKey, # UUID gbif
      INSTITUTION_CODE = institutionCode,
      LICENSE = license # this is a NEW field (a metadata of the record).
      # Is it possible to map it?
    )

  # compose species list for reference_TAXA.csv
  reference_TAXA <- lter_temp %>%
    dplyr::mutate(FIELD_NAME = "TAXA") %>%
    dplyr::select(
      FIELD_NAME,
      CODE = TAXA,
      NAME = ref_NAME,
      CODE_URL = ref_CODE_URL) %>%
    unique()

  data_mapping_LTER <- lter_temp %>%
    dplyr::select(any_of(c(
      eLTER_reporting.mapping.coreFields,
      eLTER_reporting.mapping.extendedFields,
      custom_fields
    )))

  return(list(
    deimsid = deimsid,
    source = "gbif",
    data_mapping = data_mapping_LTER,
    reference_TAXA = reference_TAXA,
    reference_VARIABLES = reference_variables_gbif
  ))
}

#' @inherit map_occ_gbif2elter
#' @export
map_occ_inat2elter <- function(x, deimsid) {
  reference_variables <- reference_variables_inat
  custom_fields <- reference_variables$VARIABLE_CODE

  # compose output for eLTER reporting format
  # (intermediate step: info both for data and for reference)
  lter_temp <- x %>%
    .add_site_code(deimsid) %>%
    # add new (computed) columns
    dplyr::mutate(
      SITE_CODE = deimsid,
      ABS_POSITION = location,
      TIME = time_observed_at,
      VARIABLE = "occurrence",
      ORG_NAME = prov,
      TAXA = taxon.id,
      VALUE = TRUE, # TRUE/FALSE for occurrence? or 0/1?
      FLAGQUA = quality_grade, # eventually add REFERENCE file describing
      # meaning for different sources
      CODE_URL = uri,
      RECORD_ID = id,
      LICENSE = license_code,
      AUTHOR_ID = user.id
    ) %>%
    # select and alias existing columns
    dplyr::select(
      SITE_CODE,
      ABS_POSITION,
      TIME,
      ORG_NAME,
      VARIABLE,
      TAXA,
      VALUE,
      FLAGQUA,
      name,
      CODE_URL,
      RECORD_ID, # this is a NEW field (a metadata of the record).
      # Is it possibile to map it?
      LICENSE = license_code,
      AUTHOR_ID
    )

  # compose species list for reference_TAXA.csv
  reference_TAXA <- lter_temp %>%
    dplyr::mutate(FIELD_NAME = "TAXA") %>%
    dplyr::select(
      FIELD_NAME,
      CODE = TAXA,
      NAME = name,
      CODE_URL
    ) %>%
    unique()

  data_mapping_LTER <- lter_temp %>%
    dplyr::select(any_of(c(
      eLTER_reporting.mapping.coreFields,
      eLTER_reporting.mapping.extendedFields,
      custom_fields
    )))

  return(list(
    deimsid = deimsid,
    source = "inat",
    data_mapping = data_mapping_LTER,
    reference_TAXA = reference_TAXA,
    reference_VARIABLES = reference_variables_inat
  ))
}

#' @inherit map_occ_gbif2elter
#' @export
map_occ_obis2elter <- function(x, deimsid) {
  # "variables" (in this case they could be defined "custom fields", being just
  # record metadata) to be described in the reference.csv file
  reference_variables <- reference_variables_obis
  custom_fields <- reference_variables$VARIABLE_CODE

  # compose output for eLTER reporting format
  # (intermediate step: info both for data and for reference)
  lter_temp <- x %>%
    .add_site_code(deimsid) %>%
    # add new (computed) columns
    dplyr::mutate(
      SITE_CODE = GOFid,
      ABS_POSITION = paste0(
        as.numeric(gsub(
          ".* ([-]*[0-9]+[.][0-9]+).*", "\\1", geometry
        )),
        " ",
        as.numeric(gsub(
          ".*?([-]*[0-9]+[.][0-9]+).*", "\\1", geometry
        )),
        " ",
        minimumDepthInMeters
      ),
      TIME = date_mid,
      VARIABLE = "occurrence",
      "individualCount", # this second variable is in field individualCount
      ORG_NAME = institutionCode,
      TAXA = aphiaID,
      VALUE = TRUE,
      "individualCount",
      CODE_URL = scientificNameID
    ) %>%
    # select and alias existing columns
    dplyr::select(
      SITE_CODE,
      ABS_POSITION,
      TIME,
      VARIABLE,
      ORG_NAME,
      TAXA,
      VALUE,
      CODE_URL,
      scientificName,
      individualCount,
      RECORD_ID = id,
      DATASET_ID = dataset_id
    )

  # compose species list for reference_TAXA.csv
  reference_TAXA <- lter_temp %>%
    dplyr::mutate(FIELD_NAME = "TAXA") %>%
    dplyr::select(
      FIELD_NAME,
      CODE = TAXA,
      NAME = scientificName,
      CODE_URL
    ) %>%
    unique()

  data_mapping_LTER <- lter_temp %>%
    dplyr::select(any_of(c(
      eLTER_reporting.mapping.coreFields,
      eLTER_reporting.mapping.extendedFields,
      custom_fields
    )))

  return(list(
    deimsid = deimsid,
    source = "obis",
    data_mapping = data_mapping_LTER,
    reference_TAXA = reference_TAXA,
    reference_VARIABLES = reference_variables_obis
  ))
}

#' creates an archive with files following the eLTER reportingFormat
#' @param lterReportOut A `list` like the one created by gbif2elter
#' @description creates a zip archive named
#' biodiv_occurrence_site_<deimsid_code>_<source>.zip
#' where <deimsid_code> is the uuid in the last part of the deimsid,
#' and <source> is one of "gbif", "inat", "obis"
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @importFrom utils zip write.csv2
#' @export
save_occ_eLTER_reporting_Archive <- function(lterReportOut) {
  sr <- lterReportOut$source
  deimsid <- lterReportOut$deimsid

  dirsr <- paste0(tempdir(), "/", sr)
  file_mapping <- paste0(dirsr, "/data_mapping.csv")
  file_reference_TAXA <- paste0(dirsr, "/reference_TAXA.csv")
  file_reference_VARIABLES <- paste0(dirsr, "/reference_VARIABLES.csv")

  # clean up existing previous work
  if (dir.exists(dirsr)) {
    file.remove(paste0(dirsr, "/", list.files(dirsr)))
    file.remove(dirsr)
  }

  dir.create(dirsr)

  write.csv2(lterReportOut$data_mapping, file_mapping)
  write.csv2(lterReportOut$reference_VARIABLES, file_reference_VARIABLES)
  write.csv2(lterReportOut$reference_TAXA, file_reference_TAXA)

  zip(
    paste0(
      "biodiv_occurrence_site_",
      .short_id(deimsid),
      "_",
      sr,
      ".zip"
    ),
  files = dirsr,
  extras = "-j"
  )
}

# from eLTER Plus tech doc 01 "Field specification for data reporting"
#' @noRd
eLTER_reporting.mapping.coreFields <- c(
  "SITE_CODE",
  "STATION_CODE",
  "ABS_POSITION",
  "VERT_OFFSET",
  "HORI_OFFSET", #horizontal offset (gbif:coordinateUncertaintyInMeters?)
  "TIME",
  "VARIABLE",
  "LAYER", # botanic only?
  "TAXA",
  "VALUE", # TRUE/FALSE for occurence? or 0/1?
  "UNIT",
  "FLAGQUA", # eventually add REFERENCE file describing meaning for
  # different sources
  "EVENT_ID",
  "SAMPLE_ID")

#' @noRd
eLTER_reporting.mapping.extendedFields <- c(
  "ORG_NAME",
  "SUBPROG",
  "MEDIUM",
  "LISTMED",
  "MAX_LEVEL",
  "MIN_LEVEL",
  "SIZE",
  "YEAR",
  "MONTH",
  "DAY",
  "HOUR",
  "MINUTE",
  "SECOND",
  "SPOOL",
  "TPOOL",
  "TLEVEL",
  "LISTTAXA",
  "LISTSUB",
  "FLAGSTA"
)

#' @noRd
reference_variables_gbif <- as_tibble(
  data.frame(
    FIELD_NAME = rep("VARIABLE", 4),
    VARIABLE_CODE = c(
      "RECORD_ID",
      "DATASET_KEY",
      "INSTITUTION_CODE",
      "LICENSE"
    ),
    VARIABLE_NAME = c(
      "gbif record id",
      "gbif datasetKey",
      "gbif institution code",
      "licence applied to each gbif record"
    ),
    VARIABLE_DEFINITION = c("", "", "", "")
  )
)

#' @noRd
reference_variables_inat <-
  as_tibble(
    data.frame(
      FIELD_NAME = rep("VARIABLE", 3),
      VARIABLE_CODE = c("RECORD_ID", "AUTHOR_ID", "LICENSE"),
      VARIABLE_NAME = c(
        "inat record id",
        "author id of inat observations",
        "licence applied to each inat record"
      ),
      VARIABLE_DEFINITION = c("", "", "")
    )
  )

#' @noRd
reference_variables_obis <- as_tibble(
  data.frame(
    FIELD_NAME = rep("VARIABLE", 3),
    VARIABLE_CODE = c("individualCount", "RECORD_ID", "DATASET_ID"),
    VARIABLE_NAME = c("individual count", "obis record id", "obis dataset id"),
    VARIABLE_DEFINITION = c("", "", "")
  )
)


#' utility function to add a column with DEIMS-SDR id to a tibble
#' @noRd
.add_site_code <- function(x, deimsid) {
  x %>% mutate(SITE_CODE = deimsid)
}

#' extract the numeric id from a DEIMS-SDR site URI
#' @noRd
.short_id <- function(deimsid) {
  strsplit(deimsid, "/")[[1]][4]
}

# # export to occurrence shapefiles
# resGarda$gbif <- resGarda$gbif %>%
#   dplyr::rename(
#     nick = `http://unknown.org/nick`,
#     captive = `http://unknown.org/captive`
#   )
# sf::st_write(
#   resGarda$gbif,
#   "~/Desktop/D4.1/dataMapping/gbif.shp",
#   append = TRUE
# )
#
# sf::st_write(
#   resGarda$inat,
#   "~/Desktop/D4.1/dataMapping/inat.shp",
#   append = TRUE
# )

#' eLTER reporting format naming convention for files
#' @description compose file name following eLTER naming convention
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @param deimsid A `character` The DEIMS ID of the site from
#' DEIMS-SDR website. More information about DEIMS ID in this pages:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @param country_code A `character` automatically evaluated if
#' DEIMS ID is provided.
#' Otherwise reference to the country of the site as two-digit country
#' code according to ISO 3166-1 alpha-2.
#' @param site_name A `character` Automatically evaluated if DEIMS ID is
#' provided.
#' Otherwise the name of the site according to DEIMS-SDR, if the name is
#' too long the site name can be shortened.
#' @param data_topic A `character`. Max 5-digit code for data topic or
#' observation programme, e.g. METEO (Meteorology), BIODIV (Biodiversity),
#' DEPO (deposition), GHG (Green House gas), SW (Soil water), VEG (Vegetation).
#' The abbreviation is defined by the data provider depending on the data.
#' @param variable_group A `character`. Optional, list of variables or
#' variable groups contained in the data. The abbreviation is defined by the
#' data provider depending on the data.
#' @param time_span A `numeric` or a `character`. Time span covered in the
#' data.
#' @param version version in format "VYYYYMMDD". Data version in the format
#' “V”YYYYMMDD.
#' Defaults to current date.
#' @return filename (without extension) following naming convention
#' @importFrom stringr str_replace_all
#' @importFrom countrycode countrycode
#' @importFrom dplyr pull
#' @seealso Peterseil, Geiger et al. (2020)
#' Field Specification for data reporting. Technical Document.
#' TechDoc.01. EU Horizon 2020 eLTER PLUS Project, Grant agreement No. 871128
#' \url{https://zenodo.org/record/6373410}
#' @note This method must be intended as a signpost for future implementation
#' @export
#' @examples
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
### function reporting_compose_file_name
reporting_compose_file_name <- function(
  deimsid = NULL,
  country_code = NULL, site_name = NULL,
  data_topic, variable_group = "",
  time_span, version = Sys.Date() %>% format("V%Y%m%d")
) {

  if (!is.null(deimsid)) {
    info <- get_site_info(deimsid)
    country_code <- info %>% dplyr::pull(country) %>% unlist() %>% .[1] %>%
      countrycode::countrycode(origin = "country.name", destination = "iso2c")
    site_name <- stringr::str_replace_all(info$title, " ", replacement = "-")
  }
  return(
    paste(country_code, site_name, data_topic, variable_group, time_span, version, sep = "_")
  )
}

#' Compose an eLTER Data Reporting Format object
#' @description Given several tables, creates an eLTER data reporting format object
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @param data A `tibble`. See eLTER data specification format for details
#' @param station A `tibble`
#' @param method A `tibble`
#' @param reference A `tibble`
#' @param event A `tibble`
#' @param sample A `tibble`
#' @param licence A `character`
#' @param data_type A `character`
#' @param filename optional filename associated with the object, of the form provided as output by
#' the function `reporting_compose_file_name`
#' @seealso Peterseil, Geiger et al. (2020) 
#' Field Specification for data reporting. Technical Document.
#' TechDoc.01. EU Horizon 2020 eLTER PLUS Project, Grant agreement No. 871128
#' \url{https://zenodo.org/record/6373410}
#' @return list with eLTER reporting format slots
#' @examples
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
#' data <- dplyr::tribble(
#'   ~id, ~value, 
#'   1, 7.5, 
#'   2, 4.2
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
#' @note This method must be intended as a signpost for future implementation
#' @export
#'
### function reporting_produce_data_object_v1.3
reporting_produce_data_object_v1.3 <- function(data = NULL, station = NULL,
                                               method = NULL, reference = NULL,
                                               event = NULL, sample = NULL,
                                               licence = "", deimsid = "",
                                               data_type = "measurement",
                                               filename = NULL) {
  if(!data_type %in% c("measurement", "mapping"))
    stop("data type must be one of measurement or mapping")
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
    LICENCE = licence
  ))
}

#' creates an archive with files following the eLTER reportingFormat
#' @description creates a zip archive <filename>.zip
#' @param x A `list` like the one created by function
#' `reporting_produce_data_object_v1.3`
#' @param filename A `character`. Optional filename associated with the object,
#' of the form provided as output by the function
#' `reporting_compose_file_name`.
#' Defaults to random string
#' @param filepath A `character` file path. Defaults to temporary directory
#' @param saveRDS A `logical`. Save also object in RDS format.
#' Defaults to FALSE
#' @return named A `list` containing paths to saved files filepaths.
#' Slots are named "zip" and possibly "RDS".
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @importFrom utils zip write.csv2
#' @importFrom stringi stri_rand_strings
#' @note This method must be intended as a signpost for future implementation
#' @seealso Peterseil, Geiger et al. (2020) 
#' Field Specification for data reporting. Technical Document.
#' TechDoc.01. EU Horizon 2020 eLTER PLUS Project, Grant agreement No. 871128
#' \url{https://zenodo.org/record/6373410}
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
#' filename <- reporting_compose_file_name(
#'   deimsid = deimsid,
#'   data_topic = data_topic,
#'   variable_group = variable_group,
#'   time_span = time_span,
#'   version = version
#' )
#' 
#' data <- dplyr::tribble(
#'   ~id, ~value, 
#'   1, 7.5, 
#'   2, 4.2
#' )
#' station <- dplyr::tribble(
#'   ~SITE_CODE, ~STATION_CODE, ~STYPE, ~LAT,      ~LON,       ~ALTITUDE,
#'   deimsid,    "IP2",         "AREA",  45.340805, 7.88887495, 265
#' )
#' 
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
#' archive <- reporting_save_archive(
#'   x = research_object,
#'   # obtained from the function `reporting_produce_data_object_v1.3()`
#'   filename = filename,
#'   # obtained from the function `reporting_compose_file_name()`
#'   filepath = ".",
#'   saveRDS = TRUE
#' )
#' }
#' 
#' ## End (Not run)
#'
### function reporting_save_archive
reporting_save_archive <- function(
  x,
  filename = NULL,
  filepath = tempdir(),
  saveRDS = FALSE
) {
  if(is.null(filename)) filename = stringi::stri_rand_strings(1, 10)
  sr <- filename
  deimsid <- x$deimsid
  
  dirsr <- paste0(filepath, "/", sr)
  
  lx <- purrr::map_lgl(x, function(y){"tbl_df" %in% class(y)})
  slotstosave <- names(lx)[lx]
  filenames <- paste0(dirsr, "/", slotstosave, ".csv")
  
  # clean up existing previous work
  if (dir.exists(dirsr)) {
    file.remove(paste0(dirsr, "/", list.files(dirsr)))
    file.remove(dirsr)
  }
  
  dir.create(dirsr)
  
  lapply(slotstosave, FUN=function(y){
    fname <- paste0(dirsr, "/", y, ".csv")
    write.csv2(x[y], fname)
  })
  
  zip(
    paste0(filepath,"/",filename,".zip"),
    files = dirsr,
    extras = "-j"
  )
  savedFiles <- list()
  savedFiles["zip"] <- paste0(filepath,"/",filename,".zip")
    
  if (saveRDS == TRUE){
    saveRDS(x, file=paste0(filepath, "/", filename, ".RDS"))
    savedFiles["RDS"] <- paste0(filepath, "/", filename, ".RDS")
  }
  
  return(savedFiles)
}

#' eLTER write rds data function
#' @description This function write a rds file from csv, tsv, txt, xls or xlsx
#' dataset
#' @param myfiles A `character`. The list of the files to deposit in Zenodo.
#' Please provide all files only with 'csv' extension.
#' @param delim A `character`. Provide the character used to separate fields
#' within a record. Only if the extension of the file(s) are 'csv', 'tsv', or
#' 'txt'.
#' @return This function returns a rds files.
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @importFrom readr read_delim
#' @keywords internal
#' @examples
#' elter_write_rdata(
#'   myfiles = c(
#'    "miscellaneus/file_show/data_mapping.csv",
#'    "miscellaneus/file_show/reference_TAXA.csv",
#'    "miscellaneus/file_show/reference_VARIABLES.csv"
#'   ),
#'   delim = ";"
#' )
#' 
### function elter_write_rdata
elter_write_rdata <- function(myfiles, delim) {
  exts <- strsplit(basename(myfiles), split="\\.")
  num_files <- length(myfiles)
  if (!all(grepl("\\.csv$", myfiles))) {
    message(
      paste0(
        "\n----\n",
        "Please provide all files with 'csv' extension.",
        "\n----\n"
      )
    )
  } else {
    files_name <- as.character()
    for (i in seq_len(num_files)) {
      name <- paste0(exts[[i]][1])
      assign(
        name,
        readr::read_delim(
          myfiles[i],
          delim = delim,
          show_col_types = FALSE
        )
      )
      files_name <- append(files_name, name)
    }
    save(
      list = files_name,
      file = "data.RData"
    )
  }
}
