#' @title eLTER_getSiteResearchTopics
#' @description This function allows to obtain the list of research topics of the eLTER site througth the DEIMS-SDR sites API.
#' @param deimsid
#' @return The output of the function is a tibble with main features of the site and the research topics envisaged into site.
#' @author Alessandro Oggioni, phD (2020) <oggioniale@gmail.com>
#' @import jsonlite
#' @export
#' @examples
#' getSiteResearchTopics(deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe')
#'
### function getResearchTopics
getSiteResearchTopics <- function(deimsid) {
  q = '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       researchTopics: .attributes.focusDesignScale.researchTopics
      }'

  url <- paste0("https://deims.org/", "api/sites/", substring(deimsid, 19))
  export <- httr::GET(url = url)
  jj <- httr::content(export, "text")
  researchTopics <- tibble::as_tibble(do_Q(q, jj))
  if (!is.na(researchTopics$researchTopics)) {
    colnames(researchTopics$researchTopics[[1]]) <- c("researchTopicsLabel", "researchTopicsUri")
  } else {
    researchTopicsLabel <- NA
    researchTopicsUri <- NA
    researchTopics$researchTopics <- list(data.frame(researchTopicsLabel, researchTopicsUri))
  }
  researchTopics
}
