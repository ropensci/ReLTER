#' @title eLTER getSiteRelatedResources function
#' @description This function allows to obtain the list of related resources
#' collected in the eLTER site througth the DEIMS-SDR sites API.
#' @param deimsid A `character`. It is the DEIMS iD of network make from
#' DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the related resources collected by site.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr GET content
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' tSiteRelatedResources <- getSiteRelatedResources(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' )
#' tSiteRelatedResources
#'
### function getSiteRelatedResources
getSiteRelatedResources <- function(deimsid) {
  q <- '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       relatedResources: .attributes.relatedResources
      }'
  url <- paste0(
    "https://deims.org/",
    "api/sites/",
    sub("^.+/", "", deimsid)
  )
  export <- httr::GET(url = url)
  jj <- suppressMessages(httr::content(export, "text"))
  status <- jj %>% 
    jqr::jq(as.character('{status: .errors.status}')) %>% 
    textConnection() %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE) %>%
    dtplyr::lazy_dt() %>% 
    dplyr::as_tibble()
  if (is.na(status)) {
    invisible(
      utils::capture.output(
        relatedResources <- dplyr::as_tibble(
          ReLTER::do_Q(q, jj)
        )
      )
    )
    if (!is.na(relatedResources$relatedResources)) {
      colnames(relatedResources$relatedResources[[1]]) <- c(
        "relatedResourcesId",
        "relatedResourcesTitle",
        "relatedResourcesChanged"
      )
    } else {
      relatedResourcesId <- NA
      relatedResourcesTitle <- NA
      relatedResourcesChanged <- NA
      relatedResources$relatedResources <- list(
        data.frame(
          relatedResourcesId,
          relatedResourcesTitle,
          relatedResourcesChanged
        )
      )
    }
  } else {
    message("\n---- The requested page could not be found. Please check again the DEIMS.iD ----\n")
    relatedResources <- NULL
  }
  relatedResources
}
