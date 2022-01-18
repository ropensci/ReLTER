#' @title eLTER produce_zenodo_record function
#' @description This function allows to deposit a dataset to
#' \href{https://zenodo.org}{Zenodo} repository. The function use the facility
#' of `zen4r`package. Blondel, Emmanuel, & Barde, Julien. (2021). zen4R: R
#' Interface to Zenodo REST API (0.5-2). Zenodo.
#' \url{https://doi.org/10.5281/zenodo.5741143}.
#' @param mytoken a `character`. Scopes assign permissions to your personal
#' access token. A personal access token works just like a normal OAuth access
#' token for authentication against the API. This token can be created
#' \href{https://zenodo.org/account/settings/applications/tokens/new/}{here}.
#' @param myfile a `character`. A file for a given Zenodo deposited record.
#' @param dataset_type a `character`. It is the type of the record to be
#' upload. Currently only "dataset" is possible, because the aim of the
#' function is create a record of dataset in Zenodo repository. Default
#' "dataset".
#' @param dataset_title a `character`. The title of the dataset.
#' @param dataset_description a `character`. The description of the dataset.
#' @param dataset_authors a `tibble`. It is a list of creator(s) of the
#' dataset. The approach is to use the firstname, lastname, affiliation,
#' orcid of the authors of the dataset.
#' @param dataset_license a `character`. Only one value is possible. It is the
#' license with which the dataset is released. The license should be set
#' with the Zenodo id of the license. Default "CC-BY-SA-4.0".
#' @param dataset_accessRight a `character`. Only one value is possible.
#' Default "open".
#' @param dataset_version a `character`. It is a version of the dataset. Only
#' one value is possible. Default "1.0".
#' @param dataset_language a `character`. It is the language of the dataset.
#' Only one value is possible. Default "eng".
#' @param dataset_keywords a `character`. A multiple values are possible. The
#' keyword to the record of dataset.
#' @param dataset_relatedIdentifier a `tibble`. It is the related entities
#' (e.g. scientific paper, data paper, etc.) with the detaset. The tibble is
#' composed by 2 variables: relation and identifier. Relation can be one of
#' among following values: "isCitedBy", "cites", "isSupplementTo",
#' "isSupplementedBy", "isNewVersionOf", "isPreviousVersionOf",
#' "isPartOf", "hasPart", "compiles", "isCompiledBy", "isIdenticalTo",
#' "isAlternateIdentifier". While identifier is any type of identifier
#' (e.g. DOI).
#' @param dataset_community a `character`. It is a name of community as created
#' in Zenodo. A multiple values are possible. Default "lter-italy".
#' @param dataset_grants a `character`. A multiple values are possible. Put a
#' list of project ids as well as showed by the European Commission via
#' \href{http://www.openaire.eu/}{OpenAIRE}.
#' @param dataset_publish a `boolean`. State whether the Zenodo record is to
#' be published. The parameter publish can be set to TRUE (to use CAUTIOUSLY,
#' only if you want to publish your record). Default FALSE.
#' @return A link (URL) of the deposited record in Zenodo. The user must then:
#' 1. visit the page of record, 2. reserve DOI by click on the button
#' 'Reserve DOI', 3. check the information provided, and 4. save and publish
#' the dataset on Zenodo repository.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom zen4R ZenodoManager
#' @export
#' @examples
#' \dontrun{
#' # This is only an example!
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
#'   # mytoken = "bWFNNtanuk8ys",
#'   mytoken = 'bWFNNtanuk6ysFb5jSaIfGjMv8OlM2X3r2vMURIt2kKvfP795tIiuL3frv9e',
#'   myfile = "data/meteo_data.csv",
#'   dataset_title = "The title",
#'   dataset_description = "This is the description of the dataset.",
#'   dataset_authors = authors,
#'   dataset_keywords = keywords,
#'   dataset_relatedIdentifier = relatedIdentifiers,
#'   dataset_grants = grants,
#'   dataset_publish = FALSE
#' )
#' }
#'
### function produce_zenodo_record
produce_zenodo_record <- function(
  mytoken,
  myfile,
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
  dataset_communities = "lter-italy",
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

  # create and fill a local not deposit yet record ----
  # myrec <- zen4R::ZenodoRecord$new()

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

  # TODO
  # prereserve DOI for record
  # myrec$prereserveDOI(prereserve = TRUE)
  # read the DOI preserved for the dataset
  # dataset_doi <- myrec$metadata$prereserve_doi$doi

  # deposit record without publish ----
  zenodo$depositRecord(myrec, publish = dataset_publish)
  # upload file to the record ----
  zenodo$uploadFile(
    myfile,
    record = myrec
  )
  #output message ----
  record_id <- myrec$record_id
  message(
    paste0(
      "\n----\n",
      "The record id for this dataset is:\n",
      record_id, "\n\n",
      "The record in Zenodo can be reached:\n",
      "https://zenodo.org/deposit/", record_id, "\n",
      "Please:\n",
      "1. visit the page of record,\n",
      "2. reserve DOI by click on the button 'Reserve DOI',\n",
      "3. check the information provided,\n",
      "4. save and publish the dataset on Zenodo repository.",
      "\n----\n"
    )
  )
  return(paste0("https://zenodo.org/deposit/", record_id))

  # TODO message if DOI is pre-reserved
  # message(
  #   paste0(
  #     "\n----\n",
  #     "The DOI preserved for this dataset is:\n",
  #     dataset_doi,"\n",
  #     "The record in Zenodo is:\n",
  #     "https://doi.org/", dataset_doi,
  #     "\n----\n"
  #   )
  # )
}
