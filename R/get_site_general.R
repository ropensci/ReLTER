#' @title eLTER get_site_general function
#' @description This function obtains general information
#' about an eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. The DEIMS.iD of the site from
#' DEIMS-SDR website. More information about DEIMS.iD from:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the general information, such as: abstract, keywords, purpose,
#' status, yearEstablished, yearClosed, hierarchy, siteName, short name, site
#' type, protection level, images.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr RETRY content
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @keywords internal
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
  jj <- get_id(deimsid, "sites")
  if (is.na(attr(jj, "status"))) {
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
