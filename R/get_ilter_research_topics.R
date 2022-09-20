#' Obtain the information about the Research Topics of ILTER sites.
#' @description `r lifecycle::badge("defunct")`
#' This function was defunct because the section
#' about research topics of the site in DEIMS-SDR API
#' version 1.1 has been removed.
#' 
#' This function obtains Research Topics as collected by all
#' \href{https://www.ilter.network/network/global-coverage}{ILTER sites
#' (more than 1200 around the world)}, as stored in
#' \href{https://deims.org}{DEIMS-SDR}. Note that the execution time for
#' this function is very high.
#'
#' If the objective is to obtain information about Research Topics
#' from a few sites, it is better to use other more specific functions (e.g.
#' \href{get_network_research_topics.html}{`get_network_research_topics()`} or
#' \href{get_site_info.html}{`get_site_info()`}) or using
#' other methods
#' (\href{../../articles/sites_information.html}{How to about sites
#' informations}).
#' @param sitesNum A `integer`. It is the number of the sites that are
#' read to get the information. Use this parameter only to get an
#' example of the output of this function. If the value of sitesNum is
#' 0 (default) all the ILTER sites will be parsed and the waiting time will
#' be long.
#' @return The output of the function is a `tibble` containing the research
#' topics and their URI (Uniform Resource Identifier) of all ILTER sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct as_tibble
#' @importFrom Rdpack reprompt
#' @references
#'   \insertRef{jsonliteR}{ReLTER}
#'
#'   \insertRef{dplyrR}{ReLTER}
#' @export
#' @examples
#' \dontrun{
#' listResearchTopics <- get_ilter_research_topics(sitesNum = 20)
#' listResearchTopics[1:10, ] %>%
#'   dplyr::rows_insert(
#'   dplyr::tibble(
#'     researchTopicsLabel = "...",
#'     researchTopicsUri = "..."
#'   )
#' )
#' }
#'
### function get_ilter_research_topics
get_ilter_research_topics <- function(sitesNum = NULL) {
  deimsbaseurl <- get_deims_base_url()
  if (is.null(sitesNum)) {
    lterILTERSites <- as.list(jsonlite::fromJSON(
      paste0(
        deimsbaseurl,
        "api/sites"
      )
    ))
    allSiteResearchTopics <- lapply(
      as.list(paste0(lterILTERSites$id$prefix, lterILTERSites$id$suffix)),
      ReLTER::get_site_info,
      category = "ResearchTop"
    )
    uniteSiteResearchTopics <- dplyr::bind_rows(allSiteResearchTopics)
    researchTopicsILTERList <- NULL #uniteSiteResearchTopics$researchTopics
    researchTopicsILTERDF <- dplyr::bind_rows(researchTopicsILTERList)
    uniqueSiteResearchTopics <- dplyr::as_tibble(
      dplyr::distinct(
        researchTopicsILTERDF
      )
    )
    uniqueSiteResearchTopics
  } else if (typeof(sitesNum) == "double") {
    lterILTERSites <- as.list(jsonlite::fromJSON(
      paste0(
        deimsbaseurl,
        "api/sites"
      )
    ))
    allSiteResearchTopics <- lapply(
      as.list(paste0(
        lterILTERSites$id$prefix[1:sitesNum],
        lterILTERSites$id$suffix[1:sitesNum]
      )),
      ReLTER::get_site_info,
      category = "ResearchTop"
    )
    uniteSiteResearchTopics <- dplyr::bind_rows(allSiteResearchTopics)
    researchTopicsILTERList <- NULL #uniteSiteResearchTopics$researchTopics
    researchTopicsILTERDF <- dplyr::bind_rows(researchTopicsILTERList)
    uniqueSiteResearchTopics <- dplyr::as_tibble(
      dplyr::distinct(
        researchTopicsILTERDF
      )
    )
    uniqueSiteResearchTopics
  } else {
    message("\n----\nThe `sitesNum` value must be a double (e.g. 10, 24, etc.).
Please check again the value of `sitesNum`.\n----\n")
    uniqueSiteResearchTopics <- NULL
  }
}
