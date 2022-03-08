#' @title eLTER get_site_parameters function
#' @description This function obtains the list of parameters measured
#' in the eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. It is the DEIMS.iD of the site from
#' DEIMS-SDR website. DEIMS.iD information 
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the parameters collected.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr RETRY content
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @keywords internal
#'
### function get_site_parameters
get_site_parameters <- function(deimsid) {
  q <- '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       parameter: .attributes.focusDesignScale.parameters
      }'
  jj <- get_id(deimsid, "sites")
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        parameters <- dplyr::as_tibble(
          do_Q(q, jj)
        )
      )
    )
    if (!is.na(parameters$parameter)) {
      colnames(parameters$parameter[[1]]) <- c(
        "parameterLabel",
        "parameterUri"
      )
    } else {
      parameterLabel <- NULL
      parameterUri <- NULL
      parameters$parameter <- list(
        data.frame(
          parameterLabel,
          parameterUri
        )
      )
    }
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS.iD\n----\n")
    parameters <- NULL
  }
  parameters
}
