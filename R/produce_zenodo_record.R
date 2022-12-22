#' Create a record in Zenodo
#' @description `r lifecycle::badge("experimental")`
#' This function allows to deposit a record to
#' \href{https://zenodo.org}{Zenodo} repository.
#' The function use the functions implemented by `zen4r` package.
#' Blondel, Emmanuel, & Barde, Julien. (2021).
#' zen4R: R Interface to Zenodo REST API (0.5-2). Zenodo.
#' \url{https://doi.org/10.5281/zenodo.5741143}.
#' @param mytoken A `character`. Scopes assign permissions to your personal
#' access token. A personal access token works just like a normal OAuth access
#' token for authentication against the API. This token can be created at
#' application
#' \href{https://zenodo.org/account/settings/applications/tokens/new/}{page}.
#' @param myfiles A `character`. The list of the file(s) to deposit in Zenodo.
#' @param delim A `character`. Provide the character used to separate fields
#' within a record. Only if you want to upload to Zenodo a dataset (see param
#' record_type below) with extension 'csv', 'tsv' or 'txt' and if you want
#' to upload also a copy of your dataset in Rdata format (see param
#' upload_rdata below).
#' @param upload_rdata A `logical`. Do you  want also upload a version of
#' the dataset in RData format onto this record?
#' Default FALSE.
#' @param record_type A `character`. A type of record among the following
#' values: 'publication', 'poster', 'presentation', 'dataset', 'image',
#' 'video', 'software', 'lesson', 'physicalobject', 'other'.
#' Default 'dataset'.
#' @param record_title A `character`. The title of the record.
#' @param record_description A `character`. The description of the record.
#' @param record_authors A `tibble`. It is a list of creator(s) of the
#' record. The approach is to use the firstname, lastname, affiliation,
#' orcid of the authors of the record. Please follow the example.
#' @param record_license A `character`. It is the license with which the
#' record is released. The license should be set with the Zenodo id of the
#' license. Default "CC-BY-SA-4.0".
#' The supported licenses values are from \url{https://opendefinition.org}
#' and \url{https://spdx.org}.
#' @param record_accessRight A `character`. Default "open". Other options are:
#' 'embargoed', 'restricted' and 'closed'.
#' @param record_version A `character`. It is a version of the record.
#' Default "1.0".
#' @param record_language A `character`. It is the language of the record.
#' Only one value is possible. Default "eng".
#' @param record_keywords A `character`. A multiple values are possible. The
#' keyword to the record of record.
#' @param record_relatedIdentifier A `tibble`. It is the related entities
#' (e.g. scientific paper, data paper, etc.) with the detaset. The tibble is
#' composed by 2 variables: relation and identifier. Relation can be one of
#' among following values: "isCitedBy", "cites", "isSupplementTo",
#' "isSupplementedBy", "isNewVersionOf", "isPreviousVersionOf",
#' "isPartOf", "hasPart", "compiles", "isCompiledBy", "isIdenticalTo",
#' "isAlternateIdentifier". While identifier is any type of identifier
#' (e.g. DOI).
#' @param record_communities A `character`. It is a name of communities as
#' created in Zenodo. A multiple values are possible. Default "lter-italy"
#' and "elter".
#' @param record_grants A `character`. A multiple values are possible. Put a
#' list of project identifier as well as showed by the European Commission via
#' \href{http://www.openaire.eu/}{OpenAIRE}.
#' @param record_publish A `logical`. State whether the Zenodo record is to
#' be published. The parameter publish can be set to TRUE (to use CAUTIOUSLY,
#' only if you want to publish your record). Default FALSE.
#' @return A link (URL) of the deposited record in Zenodo. The user must then:
#' 1. visit the webpage of record, 2. check the information provided, 3.
#' 'save' and 'publish' the record on Zenodo repository, 4. use the DOI to
#' cite the record.
#' @seealso zen4R documentation \url{https://github.com/eblondel/zen4R/wiki}
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @importFrom zen4R ZenodoManager
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#' 
#' authors <- tibble::tibble(
#'   name = c("Luke", "Leia"),
#'   surname = c("Skywalker", "Organa"),
#'   affiliation = c("Tatooine", "Alderaan"),
#'   orcid = c("0000-0002-7997-219X", "0000-0002-7997-219X")
#' )
#' keywords <- c("Star Wars", "species", "films", "planets")
#' relatedIdentifiers <- tibble::tibble(
#'   relation = c("isSupplementTo", "isPartOf"),
#'   identifier = c("10.1038/s4150-01-0032", "10.1016/j.2051.06.026")
#' )
#' grants <- c("871128", "654359", "871126")
#' produce_zenodo_record(
#'   mytoken = mytoken, # generate your Zenodo token
#'   myfiles = myfile, # provide your file(s)
#'   delim = ";",
#'   upload_rdata = TRUE,
#'   record_title = "The title",
#'   record_description = "This is the description of the record.",
#'   record_authors = authors,
#'   record_keywords = keywords,
#'   record_relatedIdentifier = relatedIdentifiers,
#'   record_communities = "lter-italy",
#'   record_grants = grants,
#'   record_publish = FALSE
#' )
#' }
#' 
#' ## End (Not run)
#'
### function produce_zenodo_record
produce_zenodo_record <- function(
  mytoken,
  myfiles,
  delim,
  upload_rdata = FALSE,
  record_type = "dataset",
  record_title,
  record_description,
  record_authors,
  record_license = "CC-BY-SA-4.0",
  record_accessRight = "open",
  record_version = "1.0",
  record_language = "eng",
  record_keywords,
  record_relatedIdentifier,
  record_communities = c("lter-italy", "elter"),
  record_grants,
  record_publish = FALSE
) {
  # open instance to Zenodo repository ----
  zenodo <- zen4R::ZenodoManager$new(
    token = mytoken,
    logger = "INFO"
  )
  # create (deposit) empty record
  myrec <- zenodo$createEmptyRecord()
  record_id <- myrec$record_id
  # populate the record ----
  myrec$setUploadType(record_type)
  myrec$setTitle(record_title)
  myrec$setDescription(record_description)
  for (m in seq_len(nrow(record_authors))) {
    myrec$addCreator(
      firstname = record_authors$name[m],
      lastname = record_authors$surname[m],
      affiliation = record_authors$affiliation[m],
      orcid = record_authors$orcid[m]
    )
  }
  myrec$setLicense(record_license)
  myrec$setAccessRight(record_accessRight)
  myrec$setVersion(record_version)
  myrec$setLanguage(record_language)
  for (n in seq_len(length(record_keywords))) {
    myrec$addKeyword(
      keyword = record_keywords[n]
    )
  }
  for (z in seq_len(nrow(record_relatedIdentifier))) {
    myrec$addRelatedIdentifier(
      relation = record_relatedIdentifier$relation[z],
      identifier = record_relatedIdentifier$identifier[z]
    )
  }
  for (j in seq_len(length(record_communities))) {
    myrec$setCommunities(
      record_communities[j]
    )
  }
  for (k in seq_len(length(record_grants))) {
    myrec$setGrants(
      record_grants[k]
    )
  }
  # write DOI reserved
  reservedDOI <- myrec$metadata$prereserve_doi$doi
  myrec$setDOI(reservedDOI)
  # deposit record without publish ----
  zenodo$depositRecord(myrec, publish = record_publish)
  # write RData file ----
  if (upload_rdata == TRUE) {
    elter_write_rdata(myfiles, delim)
    file.rename(
      from = "data.RData",
      to = paste0(record_id, ".RData")
    )
    # upload RData file to the Zenodo record ----
    zenodo$uploadFile(
      paste0(record_id, ".RData"),
      record = myrec
    )
  }
  # upload original files provided by user to the Zenodo record ----
  for (y in seq_len(length(myfiles))) {
    zenodo$uploadFile(
      myfiles[y],
      record = myrec
    )
  }
  # delete RData file
  if (upload_rdata == TRUE) {
    file.remove(paste0(record_id, ".RData"))
  }
  #output message ----
  message(
    paste0(
      "\n----\n",
      "DONE! The record is deposited on your Zenodo page!\n",
      "Now you MUST check and publish this record.\n\n",
      "The record in Zenodo can be reached at: ",
      "https://zenodo.org/deposit/", record_id, "\n\n",
      "Please:\n",
      "1. visit the webpage of record: ",
      "https://zenodo.org/deposit/", record_id, ",\n",
      "2. check the information provided,\n",
      "3. 'save' and 'publish' the record on Zenodo repository,\n",
      "4. cite the record with the DOI: ", reservedDOI,
      "\n----\n"
    )
  )
  return(paste0("https://zenodo.org/deposit/", record_id))
}

#' Create a record in Zenodo from eLTER data reporting format
#' @description `r lifecycle::badge("experimental")`
#' This function allows to deposit a record to
#' \href{https://zenodo.org}{Zenodo} repository with the eLTER data reporting
#' format.
#' The function use the functions implemented by `zen4r` package.
#' Blondel, Emmanuel, & Barde, Julien. (2021).
#' zen4R: R Interface to Zenodo REST API (0.5-2). Zenodo.
#' \url{https://doi.org/10.5281/zenodo.5741143}.
#' @param x A `list` like the one created by function
#' `reporting_produce_data_object_v1.3`
#' @param saveRDS A `logical`. Save also object in RDS format.
#' Defaults to FALSE.
#' @param filename A `character`. Optional filename associated with the object,
#' of the form provided as output by the function
#' `reporting_compose_file_name`.
#' Defaults to random string
#' @param filepath A `character` file path. Defaults to temporary directory
#' @param mytoken A `character`. Scopes assign permissions to your personal
#' access token. A personal access token works just like a normal OAuth access
#' token for authentication against the API. This token can be created at
#' application
#' \href{https://zenodo.org/account/settings/applications/tokens/new/}{page}.
#' @param record_title A `character`. The title of the record.
#' @param record_description A `character`. The description of the record.
#' @param record_authors A `tibble`. It is a list of creator(s) of the
#' record. The approach is to use the firstname, lastname, affiliation,
#' orcid of the authors of the record. Please follow the example.
#' @param record_license A `character`. It is the license with which the
#' record is released. The license should be set with the Zenodo id of the
#' license. Default "CC-BY-SA-4.0".
#' The supported licenses values are from \url{https://opendefinition.org}
#' and \url{https://spdx.org}.
#' @param record_accessRight A `character`. Default "open". Other options are:
#' 'embargoed', 'restricted' and 'closed'.
#' @param record_version A `character`. It is a version of the record.
#' Default "1.0".
#' @param record_language A `character`. It is the language of the record.
#' Only one value is possible. Default "eng".
#' @param record_keywords A `character`. A multiple values are possible. The
#' keyword to the record of record.
#' @param record_relatedIdentifier A `tibble`. It is the related entities
#' (e.g. scientific paper, data paper, etc.) with the detaset. The tibble is
#' composed by 2 variables: relation and identifier. Relation can be one of
#' among following values: "isCitedBy", "cites", "isSupplementTo",
#' "isSupplementedBy", "isNewVersionOf", "isPreviousVersionOf",
#' "isPartOf", "hasPart", "compiles", "isCompiledBy", "isIdenticalTo",
#' "isAlternateIdentifier". While identifier is any type of identifier
#' (e.g. DOI).
#' @param record_communities A `character`. It is a name of communities as
#' created in Zenodo. A multiple values are possible. Default "lter-italy"
#' and "elter".
#' @param record_grants A `character`. A multiple values are possible. Put a
#' list of project identifier as well as showed by the European Commission via
#' \href{http://www.openaire.eu/}{OpenAIRE}.
#' @return A link (URL) of the deposited record in Zenodo. The user must then:
#' 1. visit the webpage of record, 2. check the information provided, 3.
#' 'save' and 'publish' the record on Zenodo repository, 4. use the DOI to
#' cite the record.
#' @seealso zen4R documentation \url{https://github.com/eblondel/zen4R/wiki}
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#' 
#' deimsid <- "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6"
#' time_span <- 2015 # e.g. whole year
#' time_span <- "20150302-20180415" # e.g. span between two dates
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
#' authors <- tibble::tibble(
#'   name = c("Luke", "Leia"),
#'   surname = c("Skywalker", "Organa"),
#'   affiliation = c("Tatooine", "Alderaan"),
#'   orcid = c("0000-0002-7997-219X", "0000-0002-7997-219X")
#' )
#' keywords <- c("Star Wars", "species", "films", "planets")
#' relatedIdentifiers <- tibble::tibble(
#'   relation = c("isSupplementTo", "isPartOf"),
#'   identifier = c("10.1038/s4150-01-0032", "10.1016/j.2051.06.026")
#' )
#' grants <- c("871128", "654359", "871126")
#' 
#' produce_zenodo_record_from_elter_reporting(
#'   x = research_object,
#'   saveRDS = TRUE,
#'   filepath = ".",
#'   filename = filename,
#'   mytoken = mytoken, # generate your Zenodo token
#'   record_title = "The title of eLTER reporting",
#'   record_description = "This is the description of the record of eLTER data
#'     reporting format.",
#'   record_authors = authors,
#'   record_license = "CC-BY-SA-4.0",
#'   record_accessRight = "open",
#'   record_version = "1.0",
#'   record_language = "eng",
#'   record_keywords = keywords,
#'   record_relatedIdentifier = relatedIdentifiers,
#'   record_communities = "lter-italy",
#'   record_grants = grants
#' )
#' }
#' 
#' ## End (Not run)
#' 
### function produce_zenodo_record_from_elter_reporting
produce_zenodo_record_from_elter_reporting <- function(
  x,
  saveRDS = FALSE,
  filepath = tempdir(),
  filename,
  mytoken,
  record_title,
  record_description,
  record_authors,
  record_license = "CC-BY-SA-4.0",
  record_accessRight = "open",
  record_version = "1.0",
  record_language = "eng",
  record_keywords,
  record_relatedIdentifier,
  record_communities = c("lter-italy", "elter"),
  record_grants
) {
  saved_files <- reporting_save_archive(x, filename, filepath, saveRDS)
  produce_zenodo_record(
    mytoken = mytoken,
    myfiles = unlist(saved_files),
    upload_rdata = FALSE,
    record_type = "dataset",
    record_title = record_title,
    record_description = record_description,
    record_authors = record_authors,
    record_license = record_license,
    record_accessRight = record_accessRight,
    record_version = record_version,
    record_language = record_language,
    record_keywords = record_keywords,
    record_relatedIdentifier = record_relatedIdentifier,
    record_communities = record_communities,
    record_grants = record_grants,
    record_publish = FALSE
  )
}
