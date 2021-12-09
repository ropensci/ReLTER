#' @title eLTER get_site_general function
#' @description This function obtains general information
#' about an eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. More information about DEIMS ID from:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the general information, such as: abstract, keywords, purpose,
#' status, yearEstablished, yearClosed, hierarchy, siteName, short name, site
#' type, protection level, images.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr GET content
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @export
#' @keywords internal
#' @examples
#' tSiteGeneral <- get_site_general(
#'   deimsid = "https://deims.org/17210eba-d832-4759-89fa-9ff127cbdf6e"
#' )
#' tSiteGeneral
#'
### function get_site_general
get_site_general <- function(deimsid) {
  q <- '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       generalInfo: .attributes.general
      }'
  url <- paste0(
    "https://deims.org/",
    "api/sites/",
    sub("^.+/", "", deimsid)
  )
  export <- httr::GET(url = url)
  jj <- suppressMessages(httr::content(export, as="text", encoding="UTF-8"))
  status <- jj %>%
    jqr::jq(as.character("{status: .errors.status}")) %>%
    textConnection() %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE) %>%
    dtplyr::lazy_dt() %>%
    dplyr::as_tibble()
  if (is.na(status)) {
    invisible(
      utils::capture.output(
        general <- dplyr::as_tibble(
          do_Q(q, jj)
        )
      )
    )
    colnames(general$generalInfo.keywords[[1]]) <- c(
      "keywordsLabel",
      "keywordsURI"
    )
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS.iD\n----\n")
    general <- NULL
  }
  general
}
