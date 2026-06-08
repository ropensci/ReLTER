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
  affiliations <- .materialise_query(qo, deimsid, "site_affiliations")
  
  if (is.null(affiliations) || nrow(affiliations) == 0L) {
    warning("No results returned for: ", deimsid)
    return(invisible(NULL))
  }
  
  # Flatten country from list-column to vector
  affiliations$country <- unlist(affiliations$country)
  
  # Set elevation units [m]
  elev_cols <- c("geoElev.avg", "geoElev.min", "geoElev.max")
  affiliations[elev_cols] <- lapply(affiliations[elev_cols], units::set_units, value = "m")
  
  # Merge network.id.prefix and network.id.suffix into uri
  if (!is.null(affiliations$networks[[1]]) && nrow(affiliations$networks[[1]]) > 0L) {
    affiliations$networks[[1]] <- affiliations$networks[[1]] |>
      dplyr::mutate(
        name = network$name,
        uri  = paste0(network$id$prefix, network$id$suffix),
        .keep = "unused",
        .after = 1
      )
  }
  
  affiliations
}
