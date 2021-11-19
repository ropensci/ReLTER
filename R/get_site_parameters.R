#' @title eLTER get_site_parameters function
#' @description This function obtains the list of parameters measured
#' in the eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. It is the DEIMS ID of the site from
#' DEIMS-SDR website. More information about DEIMS ID from:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the parameters collected.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr GET content
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @importFrom magrittr %>%
#' @export
#' @keywords internal
#' @examples
#' tSiteParameters <- get_site_parameters(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' )
#' tSiteParameters
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
  url <- paste0(
    "https://deims.org/",
    "api/sites/",
    sub("^.+/", "", deimsid)
  )
  export <- httr::GET(url = url)
  jj <- suppressMessages(httr::content(export, "text"))
  status <- jj %>%
    jqr::jq(as.character("{status: .errors.status}")) %>%
    textConnection() %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE) %>%
    dtplyr::lazy_dt() %>%
    dplyr::as_tibble()
  if (is.na(status)) {
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
