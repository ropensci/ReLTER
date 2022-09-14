#' @title eLTER get_site_research_topics function
#' @description This function obtains a list of research topics handled
#' at an eLTER site through the DEIMS-SDR sites API.
#' @param deimsid  A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of the
#' site and a list of the research topics handled in this site.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr RETRY content
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @keywords internal
#'
### function get_site_research_topics
get_site_research_topics <- function(deimsid) {
  q <- '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       researchTopics: .attributes.focusDesignScale.researchTopics
      }'
  jj <- get_id(deimsid, "sites")
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        researchTopics <- dplyr::as_tibble(
          do_Q(q, jj)
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
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS ID\n----\n")
    researchTopics <- NULL
  }
  researchTopics
}
