#' eLTER get_site_infrastructure function
#' @description `r lifecycle::badge("stable")`
#' This internal function obtains infrastructure information
#' of an eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of the
#' site and infrastructure information where available, such as:
#' power supply, accessibility, maintenaince interval, etc.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @importFrom units set_units
#' @keywords internal
#'
### function get_site_infrastructure
get_site_infrastructure <- function(deimsid) {
  qo <- queries_jq[[get_deims_API_version()]]$site_infrastructure
  jj <- get_id(deimsid, qo$path)
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        infrastructure <- dplyr::as_tibble(do_Q(qo$query, jj))
      )
    )
    colnames(infrastructure$collection[[1]]) <- c(
      "collectionLabel",
      "collectionURI"
    )
    # set country field as vector
    infrastructure$country <- unlist(infrastructure$country)
    # set the UOM of geoElev.avg, geoElev.min, and geoElev.max
    infrastructure$geoElev.avg <- units::set_units(
      x = infrastructure$geoElev.avg,
      value = 'm'
    )
    infrastructure$geoElev.min <- units::set_units(
      x = infrastructure$geoElev.min,
      value = 'm'
    )
    infrastructure$geoElev.max <- units::set_units(
      x = infrastructure$geoElev.max,
      value = 'm'
    )
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS ID\n----\n")
    infrastructure <- NULL
  }
  infrastructure
}
