#' eLTER get_site_general function
#' @description `r lifecycle::badge("stable")`
#' This internal function obtains general information
#' about an eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the general information, such as: abstract, purpose,
#' status, yearEstablished, yearClosed, hierarchy, siteName, short name, site
#' type, protection level, images.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @importFrom units set_units
#' @keywords internal
#'
### function get_site_general
get_site_general <- function(deimsid) {
  qo <- queries_jq_deims[[get_deims_API_version()]]$site_general
  jj <- get_id(deimsid, qo$path)
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        general <- dplyr::as_tibble(do_Q(qo$query, jj))
      )
    )
    # set country field as vector
    general$country <- unlist(general$country)
    # set the UOM of geoElev.avg, geoElev.min, and geoElev.max
    general$geoElev.avg <- units::set_units(
      x = general$geoElev.avg,
      value = 'm'
    )
    general$geoElev.min <- units::set_units(
      x = general$geoElev.min,
      value = 'm'
    )
    general$geoElev.max <- units::set_units(
      x = general$geoElev.max,
      value = 'm'
    )
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS ID\n----\n")
    general <- NULL
  }
  general
}
