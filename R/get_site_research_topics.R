#' eLTER get_site_research_topics function
#' @description `r lifecycle::badge("defunct")`
#' This internal function was defunct because the section
#' about research topics of the site in DEIMS-SDR API
#' version 1.1 has been removed.
#' 
#' This function obtains a list of research topics handled
#' at an eLTER site through the DEIMS-SDR sites API.
#' @param deimsid  A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of the
#' site and a list of the research topics handled in this site.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @importFrom lifecycle deprecate_warn
#' @keywords internal
#'
### function get_site_research_topics
get_site_research_topics <- function(deimsid) {
  lifecycle::deprecate_warn("1.2.0", "get_site_research_topics()")
  get_site_info(deimsid = deimsid)
#   qo <- queries_jq[[get_deims_API_version()]]$site_researchTopics
#   jj <- get_id(deimsid, qo$path)
#   if (is.na(attr(jj, "status"))) {
#     invisible(
#       utils::capture.output(
#         researchTopics <- dplyr::as_tibble(do_Q(qo$query, jj))
#       )
#     )
#     if (!is.na(researchTopics$researchTopics)) {
#       colnames(researchTopics$researchTopics[[1]]) <- c(
#         "researchTopicsLabel",
#         "researchTopicsUri"
#       )
#     } else {
#       researchTopicsLabel <- NA
#       researchTopicsUri <- NA
#       researchTopics$researchTopics <- list(
#         data.frame(
#           researchTopicsLabel,
#           researchTopicsUri
#         )
#       )
#     }
#   } else {
#     message("\n----\nThe requested page could not be found.
# Please check again the DEIMS ID\n----\n")
#     researchTopics <- NULL
#   }
#   researchTopics
}
