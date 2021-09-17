#' @title eLTER getSiteResearchTopics function
#' @description This function allows to obtain the list of research topics of
#' the eLTER site througth the DEIMS-SDR sites API.
#' @param deimsid  A `character`. It is the DEIMS iD of network make from
#' DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the research topics envisaged into site.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import tibble httr
#' @export
#' @examples
#' tSiteResearchTopics <- getSiteResearchTopics(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' )
#' tSiteResearchTopics
#'
### function getResearchTopics
getSiteResearchTopics <- function(deimsid) {
  q <- '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       researchTopics: .attributes.focusDesignScale.researchTopics
      }'

  url <- paste0("https://deims.org/", "api/sites/", substring(deimsid, 19))
  export <- httr::GET(url = url)
  jj <- suppressMessages(httr::content(export, "text"))
  invisible(capture.output(researchTopics <- tibble::as_tibble(
    ReLTER::do_Q(q, jj)
  )))
  if (!is.na(researchTopics$researchTopics)) {
    colnames(researchTopics$researchTopics[[1]]) <- c(
      "researchTopicsLabel",
      "researchTopicsUri"
    )
  } else {
    researchTopicsLabel <- NA
    researchTopicsUri <- NA
    researchTopics$researchTopics <- list(
      data.frame(
        researchTopicsLabel,
        researchTopicsUri
      )
    )
  }
  researchTopics
}
