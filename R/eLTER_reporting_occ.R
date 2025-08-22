#' Harmonize outputs of get_site_speciesOccurrence and map them into eLTER
#' reporting format
#' @description `r lifecycle::badge("experimental")`
#' @param x A `tibble` like one that can be obtained by
#' as_tibble(get_site_speciesOccurrences(deimsid, "gbif")$gbif)
#' @param deimsid A `character`. The DEIMS.iD of the site from
#' DEIMS-SDR website. DEIMS ID information
#' @param version A `character` for select the version of eLTER
#' Data Reporting Format. Default 1.3
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
map_occ_gbif2elter <- function(x, deimsid, version = '1.3') {
  # "variables" (in this case they could be defined "custom fields",
  # being just record metadata)
  # to be described in the reference.csv file
  reference_variables <- reference_variables_gbif
  custom_fields <- reference_variables$VARIABLE_CODE
  
  # compose output for eLTER reporting format
  # (intermediate step: info both for data and for reference)
  if ("eventTime" %in% names(x)) {
    x <- x %>%
      dplyr::mutate(
        time = paste0(eventDate, "T", eventTime)
      )
  } else {
    x <- x %>%
      dplyr::mutate(
        time = eventDate
      )
  }
  lter_temp <- x %>%
    .add_site_code(deimsid) %>%
    # add new (computed) columns
    dplyr::mutate(
      VARIABLE = "occurrence",
      VALUE = TRUE,
      TIME = time,
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
  
  v <- paste0("version", version)
  data_mapping_LTER <- lter_temp %>%
    dplyr::select(any_of(c(
      eLTER_data_reporting_format[[v]]$DATA$mapping_coreFields,
      eLTER_data_reporting_format[[v]]$DATA$extendedFields,
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
map_occ_inat2elter <- function(x, deimsid, version = '1.3') {
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
  
  v <-  paste0("version", version)
  data_mapping_LTER <- lter_temp %>%
    dplyr::select(any_of(c(
      eLTER_data_reporting_format[[v]]$DATA$mapping_coreFields,
      eLTER_data_reporting_format[[v]]$DATA$extendedFields,
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
map_occ_obis2elter <- function(x, deimsid, version = "1.3") {
  # "variables" (in this case they could be defined "custom fields", being just
  # record metadata) to be described in the reference.csv file
  reference_variables <- reference_variables_obis
  custom_fields <- reference_variables$VARIABLE_CODE
  
  # compose output for eLTER reporting format
  # (intermediate step: info both for data and for reference)
  lter_temp <- x %>%
    # add new (computed) columns
    dplyr::mutate(
      SITE_CODE = deimsid,
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
  
  v <-  paste0("version", version)
  data_mapping_LTER <- lter_temp %>%
    dplyr::select(any_of(c(
      eLTER_data_reporting_format[[v]]$DATA$mapping_coreFields,
      eLTER_data_reporting_format[[v]]$DATA$extendedFields,
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

#' Creates an archive with files following the eLTER reportingFormat
#' @description `r lifecycle::badge("experimental")`
#' Creates a zip archive named
#' biodiv_occurrence_site_"deimsid_code"_"source".zip
#' where "deimsid_code" is the uuid in the last part of the deimsid,
#' and "source" is one of "gbif", "inat", "obis"
#' @param lterReportOut A `list` like the one created by `map_occ_gbif2elter`
#' @param path path of the zip file. Defaults to temporary folder
#' @return the path to the created file
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @importFrom utils zip write.csv2
#' @export
save_occ_eLTER_reporting_Archive <- function(lterReportOut, path = tempdir()) {
  if(!endsWith(path,"/"))
    path <- paste0(path,"/")
  if(!dir.exists(path))
    stop("specified directory does not exists")
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
  
  zipname<-paste0(
    "biodiv_occurrence_site_",
    .short_id(deimsid),
    "_",
    sr,
    ".zip"
  )
  filepath <- paste0(path, zipname)
  
  zip(zipfile = filepath, files = dirsr, extras = "-j")
  
  return(filepath)
}

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
  x %>% dplyr::mutate(SITE_CODE = deimsid)
}

#' extract the numeric id from a DEIMS-SDR site URI
#' @noRd
.short_id <- function(deimsid) {
  strsplit(deimsid, "/")[[1]][4]
}
