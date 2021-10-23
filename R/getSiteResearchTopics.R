#' @title eLTER getSiteResearchTopics function
#' @description This function obtains a list of research topics handled 
#' at an eLTER site througth the DEIMS-SDR sites API.
#' @param deimsid  A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. More information about DEIMS ID from:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features of the
#' site and a list of the research topics handled in this site.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr GET content
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' tSiteResearchTopics <- getSiteResearchTopics(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' )
#' tSiteResearchTopics
#'
### function getResearchTopics
getSiteResearchTopics <- function(deimsid) {
  require(dplyr)
  q <- '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       researchTopics: .attributes.focusDesignScale.researchTopics
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
        researchTopics <- dplyr::as_tibble(
          ReLTER:::do_Q(q, jj)
        )
      )
    )
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
  } else {
    message("\n---- The requested page could not be found. Please check again the DEIMS.iD ----\n")
    researchTopics <- NULL
  }
  researchTopics
}
