#' Upload dataset to Zenodo
#' @description `r lifecycle::badge("experimental")`
#' This function allows to deposit a dataset (only one dataset at the moment)
#' to \href{https://zenodo.org}{Zenodo} repository. The function use the 
#' facilities of `zen4r` package. Blondel, Emmanuel, & Barde, Julien. (2021).
#' zen4R: R Interface to Zenodo REST API (0.5-2). Zenodo.
#' \url{https://doi.org/10.5281/zenodo.5741143}.
#' @param mytoken A `character`. Scopes assign permissions to your personal
#' access token. A personal access token works just like a normal OAuth access
#' token for authentication against the API. This token can be created at
#' application
#' \href{https://zenodo.org/account/settings/applications/tokens/new/}{page}.
#' @param myfiles A `character`. The list of the files to deposit in Zenodo.
#' Please provide all files only with 'csv' extension.
#' @param delim A `character`. Provide the character used to separate fields
#' within a record. Only if the extension of the file(s) are 'csv', 'tsv', or
#' 'txt'.
#' @param elter_data_format A `logical`. Is the dataset in the elter data
#' reporting format? Default TRUE.
#' @param upload_rdata A `logical`. Do you  want also upload a version of
#' the dataset in RData format onto this record?
#' Choosing this option you will have a record improvement ...
#' Default TRUE.
#' @param dataset_title A `character`. The title of the dataset.
#' @param dataset_description A `character`. The description of the dataset.
#' @param dataset_authors A `tibble`. It is a list of creator(s) of the
#' dataset. The approach is to use the firstname, lastname, affiliation,
#' orcid of the authors of the dataset. Please follow the example.
#' @param dataset_license A `character`. It is the license with which the
#' dataset is released. The license should be set with the Zenodo id of the
#' license. Default "CC-BY-SA-4.0".
#' The supported licenses values are from \url{https://opendefinition.org}
#' and \url{https://spdx.org}.
#' @param dataset_accessRight A `character`. Default "open". Other options are:
#' 'embargoed', 'restricted' and 'closed'.
#' @param dataset_version A `character`. It is a version of the dataset.
#' Default "1.0".
#' @param dataset_language A `character`. It is the language of the dataset.
#' Only one value is possible. Default "eng".
#' @param dataset_keywords A `character`. A multiple values are possible. The
#' keyword to the record of dataset.
#' @param dataset_relatedIdentifier A `tibble`. It is the related entities
#' (e.g. scientific paper, data paper, etc.) with the detaset. The tibble is
#' composed by 2 variables: relation and identifier. Relation can be one of
#' among following values: "isCitedBy", "cites", "isSupplementTo",
#' "isSupplementedBy", "isNewVersionOf", "isPreviousVersionOf",
#' "isPartOf", "hasPart", "compiles", "isCompiledBy", "isIdenticalTo",
#' "isAlternateIdentifier". While identifier is any type of identifier
#' (e.g. DOI).
#' @param dataset_community A `character`. It is a name of community as created
#' in Zenodo. A multiple values are possible. Default "lter-italy" and "elter".
#' @param dataset_grants A `character`. A multiple values are possible. Put a
#' list of project identifier as well as showed by the European Commission via
#' \href{http://www.openaire.eu/}{OpenAIRE}.
#' @param dataset_publish A `logical`. State whether the Zenodo record is to
#' be published. The parameter publish can be set to TRUE (to use CAUTIOUSLY,
#' only if you want to publish your record). Default FALSE.
#' @return A link (URL) of the deposited record in Zenodo. The user must then:
#' 1. visit the webpage of record, 2. check the information provided, 3.
#' 'save' and 'publish' the dataset on Zenodo repository, 4. use the DOI to
#' cite the record.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
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
#'   mytoken = 'HOwWaUAel6C4j5UkOgWlLGCSkAkkiKuTCjlqJcfQ16OvGCjIL9SrLQBC1hAU',
#'   myfiles = c(
#'    "miscellaneus/file_show/data_mapping.csv",
#'    "miscellaneus/file_show/reference_TAXA.csv",
#'    "miscellaneus/file_show/reference_VARIABLES.csv"
#'   ),
#'   delim = ";",
#'   elter_data_format = TRUE,
#'   upload_rdata = TRUE,
#'   dataset_title = "The title",
#'   dataset_description = "This is the description of the dataset.",
#'   dataset_authors = authors,
#'   dataset_keywords = keywords,
#'   dataset_relatedIdentifier = relatedIdentifiers,
#'   dataset_communities = "lter-italy",
#'   dataset_grants = grants,
#'   dataset_publish = FALSE
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
  elter_data_format = TRUE,
  upload_rdata = TRUE,
  dataset_type = "dataset",
  dataset_title,
  dataset_description,
  dataset_authors,
  dataset_license = "CC-BY-SA-4.0",
  dataset_accessRight = "open",
  dataset_version = "1.0",
  dataset_language = "eng",
  dataset_keywords,
  dataset_relatedIdentifier,
  dataset_communities = c("lter-italy", "elter"),
  dataset_grants,
  dataset_publish = FALSE
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
  myrec$setUploadType(dataset_type)
  myrec$setTitle(dataset_title)
  myrec$setDescription(dataset_description)
  for (m in seq_len(nrow(dataset_authors))) {
    myrec$addCreator(
      firstname = dataset_authors$name[m],
      lastname = dataset_authors$surname[m],
      affiliation = dataset_authors$affiliation[m],
      orcid = dataset_authors$orcid[m]
    )
  }
  myrec$setLicense(dataset_license)
  myrec$setAccessRight(dataset_accessRight)
  myrec$setVersion(dataset_version)
  myrec$setLanguage(dataset_language)
  for (n in seq_len(length(dataset_keywords))) {
    myrec$addKeyword(
      keyword = dataset_keywords[n]
    )
  }
  for (z in seq_len(nrow(dataset_relatedIdentifier))) {
    myrec$addRelatedIdentifier(
      relation = dataset_relatedIdentifier$relation[z],
      identifier = dataset_relatedIdentifier$identifier[z]
    )
  }
  for (j in seq_len(length(dataset_communities))) {
    myrec$setCommunities(
      dataset_communities[j]
    )
  }
  for (k in seq_len(length(dataset_grants))) {
    myrec$setGrants(
      dataset_grants[k]
    )
  }
  # write DOI reserved
  reservedDOI <- myrec$metadata$prereserve_doi$doi
  myrec$setDOI(reservedDOI)
  # deposit record without publish ----
  zenodo$depositRecord(myrec, publish = dataset_publish)
  
  # IDEA The Rdata should contain all the table of eLTER data reporting format
  # with the semantic enrichment.
  # QUESTION How can add URI of the columns term directly from information
  # provided in eLTER data reporting format? If this were possible, could add
  # directly the URI as an attribute (see idea for semantic
  # enrichment) to a tibble in Rdata.
  # IDEA The obtained Rdata can uploaded to the same Zenodo record.

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
  file.remove(paste0(record_id, ".RData"))
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
      "3. 'save' and 'publish' the dataset on Zenodo repository,\n",
      "4. cite the record with the DOI: ", reservedDOI,
      "\n----\n"
    )
  )
  return(paste0("https://zenodo.org/deposit/", record_id))
}
