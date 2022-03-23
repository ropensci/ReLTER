#' Harmonize outputs of get_site_speciesOccurrence and map them into eLTER reporting format
#' @param x Tibble like one that can be obtained by
#'   as_tibble(get_site_speciesOccurrence(deimsid, "gbif")$gbif)
#' @param deimsid deims id of the site of interest
#' @return list with the following named elements:
#'   * deimsid: the same deimsid passed in input
#'   * source: one of "gbif", "inat", "obis"
#'   * data_mapping: tibble structured according to data_mapping of 
#'      eLTER reporting format
#'   * reference_TAXA: tibble structured according to reference_TAXA of 
#'      eLTER reporting format
#'   * reference_VARIABLES: tibble structured according to reference_VARIABLES 
#'      of eLTER reporting format
#' @author Paolo Tagliolato, phD (2020) \email{tagliolato.p@@irea.cnr.it}
#' @importFrom tibble as_tibble
#' @importFrom dplyr select mutate filter
#' @export
map_occ_gbif2elter <- function(x, deimsid){
  # "variables" (in this case they could be defined "custom fields", 
  # being just record metadata) 
  # to be described in the reference.csv file
  reference_variables<-reference_variables_gbif
  # reference_variables<-as_tibble(data.frame(
  #   FIELD_NAME=rep("VARIABLE",4),
  #   VARIABLE_CODE=c("RECORD_ID","DATASET_KEY","INSTITUTION_CODE","LICENSE"),
  #   VARIABLE_NAME=c("gbif record id",
  #                   "gbif datasetKey", 
  #                   "gbif institution code", 
  #                   "licence applied to each gbif record"),
  #   VARIABLE_DEFINITION=c("","","","")
  # ))
  
  custom_fields=reference_variables$VARIABLE_CODE
  
  # compose output for eLTER reporting format 
  # (intermediate step: info both for data and for reference)
  lter_temp <- x %>% .add_site_code(deimsid) %>% 
    # add new (computed) columns
    dplyr::mutate(VARIABLE="occurrence", 
                  VALUE=TRUE, 
                  TIME=paste0(eventDate,"T",eventTime),
                  ref_CODE_URL=paste0("https://www.gbif.org/species/",taxonKey)) %>%
    # select and alias existing columns
    dplyr::select(
      SITE_CODE,
      ABS_POSITION=geometry,
      TAXA = taxonKey, 
      VARIABLE , 
      VALUE, 
      TIME,
      ORG_NAME=prov,
      ref_NAME=acceptedScientificName,
      ref_CODE_URL,
      # custom fields/variables
      RECORD_ID=key, #tblGarda_gbif$institutionCode
      DATASET_KEY=datasetKey, # UUID gbif
      INSTITUTION_CODE=institutionCode,
      LICENSE=license # this is a NEW field (a metadata of the record). Is it possibile to map it?
    )
  
  # compose species list for reference_TAXA.csv
  reference_TAXA<-lter_temp %>% 
    mutate(FIELD_NAME="TAXA") %>%
    select(FIELD_NAME,CODE=TAXA, NAME=ref_NAME, 
           CODE_URL=ref_CODE_URL) %>% unique()
  
  data_mapping_LTER <- lter_temp %>% 
    dplyr::select(any_of(c(
      eLTER_reporting.mapping.coreFields,
      eLTER_reporting.mapping.extendedFields,
      custom_fields
    )))
  
  return(list(deimsid=deimsid,
              source="gbif",
              data_mapping=data_mapping_LTER,
              reference_TAXA=reference_TAXA,
              reference_VARIABLES=reference_variables_gbif))
}

#' @inherit map_occ_gbif2elter
#' @export
map_occ_inat2elter <- function(x, deimsid){
  reference_variables<-reference_variables_inat
  # "variables" (in this case they could be defined "custom fields", being just record metadata) to be described in the reference.csv file
  # as_tibble(reference_variables_inat<-data.frame(FIELD_NAME=rep("VARIABLE",3),
  #                                      VARIABLE_CODE=c("RECORD_ID","AUTHOR_ID","LICENSE"),
  #                                      VARIABLE_NAME=c("inat record id","author id of inat observations", "licence applied to each inat record"),
  #                                      VARIABLE_DEFINITION=c("","","")
  # ))
  
  custom_fields=reference_variables$VARIABLE_CODE
  
  # compose output for eLTER reporting format 
  # (intermediate step: info both for data and for reference)
  lter_temp <- x %>% .add_site_code(deimsid) %>%
    # add new (computed) columns
    dplyr::mutate(
      SITE_CODE = gardaid,
      ABS_POSITION = location,
      TIME = time_observed_at,
      VARIABLE = "occurrence",
      ORG_NAME = prov,
      TAXA = taxon.id,
      VALUE = TRUE, # TRUE/FALSE for occurrence? or 0/1?
      FLAGQUA = quality_grade, # eventually add REFERENCE file describing meaning for different sources
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
      RECORD_ID, # this is a NEW field (a metadata of the record). Is it possibile to map it?
      LICENSE = license_code,
      AUTHOR_ID
    )
  
  # compose species list for reference_TAXA.csv
  reference_TAXA<-lter_temp %>% 
    mutate(FIELD_NAME="TAXA") %>%
    select(FIELD_NAME,CODE=TAXA,NAME=name,CODE_URL) %>% unique()
  
  data_mapping_LTER <- lter_temp %>% 
    dplyr::select(any_of(c(
      eLTER_reporting.mapping.coreFields,
      eLTER_reporting.mapping.extendedFields,
      custom_fields
    )))
  
  return(list(deimsid=deimsid,
              source="inat",
              data_mapping=data_mapping_LTER,
              reference_TAXA=reference_TAXA,
              reference_VARIABLES=reference_variables_inat))
}

#' @inherit map_occ_gbif2elter
#' @export
map_occ_obis2elter <- function(x, deimsid){
  # "variables" (in this case they could be defined "custom fields", being just record metadata) to be described in the reference.csv file
  reference_variables<-reference_variables_obis
  # reference_variables_obis<-tibble(data.frame(FIELD_NAME=rep("VARIABLE",3),
  #                                      VARIABLE_CODE=c("individualCount","RECORD_ID","DATASET_ID"),
  #                                      VARIABLE_NAME=c("individual count","obis record id","obis dataset id"),
  #                                      VARIABLE_DEFINITION=c("","","")
  # ))
  
  custom_fields=reference_variables$VARIABLE_CODE
  
  # compose output for eLTER reporting format 
  # (intermediate step: info both for data and for reference)
  lter_temp <- x %>% .add_site_code(deimsid) %>%
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
  reference_TAXA<-lter_temp %>% 
    mutate(FIELD_NAME="TAXA") %>%
    select(FIELD_NAME,CODE=TAXA,NAME=scientificName,CODE_URL) %>% unique()
  
  data_mapping_LTER <- lter_temp %>% 
    dplyr::select(any_of(c(
      eLTER_reporting.mapping.coreFields,
      eLTER_reporting.mapping.extendedFields,
      reference_variables_obis$VARIABLE_CODE
      #custom_fields_obis
    )))
  
  return(list(deimsid=deimsid,
              source="obis",
              data_mapping=data_mapping_LTER,
              reference_TAXA=reference_TAXA,
              reference_VARIABLES=reference_variables_obis))
}

#' creates an archive with files following the eLTER reportingFormat
#' @param eLterReportOut a list like the one created by gbif2elter
#' @description creates a zip archive named 
#' biodiv_occurrence_site_<deimsid_code>_<source>.zip
#' where <deimsid_code> is the uuid in the last part of the deimsid, 
#' and <source> is one of "gbif", "inat", "obis"
#' @author Paolo Tagliolato, phD (2020) \email{tagliolato.p@@irea.cnr.it}
#' @importFrom utils zip write.csv2
#' @export
save_occ_eLTER_reporting_Archive<-function(lterReportOut){
  sr=lterReportOut$source
  deimsid=lterReportOut$deimsid
  
  dirsr<-paste0(tempdir(),"/",sr)
  file_mapping<-paste0(dirsr,"/data_mapping.csv")
  file_reference_TAXA<-paste0(dirsr,"/reference_TAXA.csv")
  file_reference_VARIABLES<-paste0(dirsr,"/reference_VARIABLES.csv")
  
  # clean up existing previous work
  if(dir.exists(dirsr)){
    file.remove(paste0(dirsr,"/",list.files(dirsr)))
    file.remove(dirsr)
  }
  
  dir.create(dirsr)
  
  write.csv2(lterReportOut$data_mapping,file_mapping)
  write.csv2(lterReportOut$reference_VARIABLES,file_reference_VARIABLES)
  write.csv2(lterReportOut$reference_TAXA,file_reference_TAXA)
  
  zip(paste0("biodiv_occurrence_site_",.short_id(deimsid),"_",sr,".zip"),
      files = dirsr,
      extras="-j")
}

# from eLTER Plus tech doc 01 "Field specification for data reporting"
#' @noRd
eLTER_reporting.mapping.coreFields<-c(
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
  "FLAGQUA", # eventually add REFERENCE file describing meaning for different sources
  "EVENT_ID",
  "SAMPLE_ID")

#' @noRd
eLTER_reporting.mapping.extendedFields<-c("ORG_NAME",
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
                                          "FLAGSTA")


#' @noRd
reference_variables_gbif<-
  as_tibble(
    data.frame(
      FIELD_NAME=rep("VARIABLE",4),
      VARIABLE_CODE=c("RECORD_ID","DATASET_KEY","INSTITUTION_CODE","LICENSE"),
      VARIABLE_NAME=c("gbif record id","gbif datasetKey", 
                      "gbif institution code", 
                      "licence applied to each gbif record"),
      VARIABLE_DEFINITION=c("", "", "", "")
    ))

#' @noRd
reference_variables_inat<-
  as_tibble(
    data.frame(
      FIELD_NAME = rep("VARIABLE",3),
      VARIABLE_CODE = c("RECORD_ID","AUTHOR_ID","LICENSE"),
      VARIABLE_NAME = c("inat record id","author id of inat observations", "licence applied to each inat record"),
      VARIABLE_DEFINITION = c("","","")
    ))

#' @noRd
reference_variables_obis <- as_tibble(
  data.frame(
    FIELD_NAME = rep("VARIABLE", 3),
    VARIABLE_CODE = c("individualCount", "RECORD_ID", "DATASET_ID"),
    VARIABLE_NAME = c("individual count", "obis record id", "obis dataset id"),
    VARIABLE_DEFINITION = c("", "", "")
  ))


#' utility function to add a column with deims id to a tibble
#' @noRd
.add_site_code <- function(x, deimsid){
  x %>% mutate(
    SITE_CODE = deimsid
  )
}

#' extract the numeric id from a deims site URI
#' @noRd
.short_id <- function(deimsid) {
  strsplit(deimsid,"/")[[1]][4]
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
