# Suppress R CMD check NOTEs for variables used in dplyr/tidyverse pipelines
utils::globalVariables(c(
  # already present
  "freq",
  "hjust",
  "label",
  "middle",
  "n",
  "parameterGroups",
  "vjust",
  "end",
  "start",
  "perc",
  "prod_opt_list",
  # shiny functions used conditionally (shiny in Suggests)
  "reactive",
  "observeEvent",
  "req",
  # dplyr tidyselect
  "any_of",
  # eLTER_reporting_occ.R
  "ABS_POSITION",
  "acceptedScientificName",
  "aphiaID",
  "AUTHOR_ID",
  "CODE_URL",
  "dataset_id",
  "datasetKey",
  "date_mid",
  "eLTER_data_reporting_format",
  "eventDate",
  "eventTime",
  "FIELD_NAME",
  "FLAGQUA",
  "geometry",
  "id",
  "individualCount",
  "institutionCode",
  "key",
  "license_code",
  "location",
  "minimumDepthInMeters",
  "name",
  "prov",
  "quality_grade",
  "RECORD_ID",
  "ref_CODE_URL",
  "ref_NAME",
  "scientificName",
  "scientificNameID",
  "SITE_CODE",
  "taxon.id",
  "taxonKey",
  "TIME",
  "time_observed_at",
  "uri",
  "user.id",
  "VALUE",
  "VARIABLE",
  "TAXA",
  # get_zenodo_data.R
  "affiliation",
  "filesize",
  "orcid",
  # get_sites_within_radius.R
  "bbx",
  # taxon_id_pesi.R
  "accordingTo",
  "authorship",
  "canonicalName",
  "checkStatus",
  "LSID",
  # taxon_id_worms.R
  "matchType",
  "status",
  "valid_AphiaID",
  "valid_authority",
  "valid_name",
  # get_location_info.R
  "locationType",
  "relatedSite.title",
  # get_site_affiliations.R
  "network",
  # get_site_related_resources.R
  "relatedResourcesChanged",
  "relatedResourcesId",
  # get_site_speciesOccurrences.R / eLTER_reporting_occ.R
  "ORG_NAME",
  # produce_site_observedProperties_waffle.R
  "group",
  "x",
  "y"
))

#' Package settings that can be changed by the user
#' @description `r lifecycle::badge("stable")`
#' @family package_customizable_settings
#' @export
package_settings <- (function() {
  pe <- new.env()
  deimsBaseUrl <- "https://deims.org/"
  assign("deimsBaseUrl", deimsBaseUrl, envir = pe)
  pe
})()

#' Set DEIMS-SDR API base URL
#' @description `r lifecycle::badge("stable")`
#' @param url A `character`. Set the base URL to DEIMS-SDR.
#' @param force A `boolean`. Default FALSE.
#' @export
#' @importFrom httr2 request req_method req_headers
#' @importFrom httr2 req_retry req_perform
set_deims_base_url <- function(url = "https://deims.org/", force = FALSE) {
  if (!endsWith(url, "/")) {
    url <- paste0(url, "/")
  }
  
  check <- tryCatch(
    {
      export <- httr2::request(url) %>%
        httr2::req_method("GET") %>%
        httr2::req_headers(Accept = "application/html") %>%
        httr2::req_retry(max_tries = 3, max_seconds = 120)
      export <- httr2::req_method(export, "HEAD")
      httr2::req_perform(export)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )

  if (check == FALSE) {
    if (force) {
      warning("The URL ", url, " is not reachable, I set it because
              force TRUE is specified")
    } else  {
      stop("The URL ", url, " is not reachable")
    }
  }
  message("Changing DEIMS-SDR base URL to: ", url)
  assign("deimsBaseUrl", url, envir = package_settings)
}

#' Get DEIMS-SDR base URL
#' @description `r lifecycle::badge("stable")`
#' @return DEIMS-SDR base URL
#' @family package_customizable_settings
#' @export
get_deims_base_url <- function() {
  get("deimsBaseUrl", envir = package_settings)
}
