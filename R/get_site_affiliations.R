#' eLTER get_site_affiliations function
#' @description `r lifecycle::badge("stable")`
#' This internal function obtains details about an eLTER site
#' through the DEIMS-SDR sites API.
#' @param deimsid A character. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the affiliations information, such as: networks and projects in
#' which the site is involved.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom dplyr as_tibble mutate
#' @importFrom units set_units
#' @importFrom utils capture.output
#' @keywords internal
#'
### function get_site_affiliations
get_site_affiliations <- function(deimsid) {
  qo <- queries_jq_deims[[get_deims_API_version()]]$site_affiliations
  jj <- get_id(deimsid, qo$path)
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        affiliations <- dplyr::as_tibble(do_Q(qo$query, jj))
      )
    )
    # set country field as vector
    affiliations$country <- unlist(affiliations$country)
    # set the UOM of geoElev.avg, geoElev.min, and geoElev.max
    affiliations$geoElev.avg <- units::set_units(
      x = affiliations$geoElev.avg,
      value = 'm'
    )
    affiliations$geoElev.min <- units::set_units(
      x = affiliations$geoElev.min,
      value = 'm'
    )
    affiliations$geoElev.max <- units::set_units(
      x = affiliations$geoElev.max,
      value = 'm'
    )
    # merge network.id.prefix with network.id.suffix
    affiliations$networks[[1]] <- affiliations$networks[[1]] %>%
      dplyr::mutate(
        name = network$name,
        uri = paste0(network$id$prefix,
                     network$id$suffix),
        .keep = "unused",
        .after = 1
      )
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS ID\n----\n")
    affiliations <- NULL
  }
  affiliations
}
