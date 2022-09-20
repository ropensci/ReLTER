#' @title eLTER get_site_parameters function
#' @description This function obtains the list of observed properties measured
#' in the eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. It is the DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the observed properties collected.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr RETRY content
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @keywords internal
#'
### function get_site_observedProperties
get_site_observedProperties <- function(deimsid) {
  q <- '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       observedProperties: .attributes.focusDesignScale.observedProperties
      }'
  jj <- get_id(deimsid, "sites")
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        observedProperties <- dplyr::as_tibble(
          do_Q(q, jj)
        )
      )
    )
    if (!is.na(observedProperties$observedProperties)) {
      colnames(observedProperties$observedProperties[[1]]) <- c(
        "observedPropertiesLabel",
        "observedPropertiesUri"
      )
    } else {
      observedPropertiesLabel <- NULL
      observedPropertiesUri <- NULL
      observedProperties$observedProperties <- list(
        data.frame(
          observedPropertiesLabel,
          observedPropertiesUri
        )
      )
    }
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS ID\n----\n")
    observedProperties <- NULL
  }
  observedProperties
}
