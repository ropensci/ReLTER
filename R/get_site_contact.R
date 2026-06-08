#' eLTER get_site_contact function
#' @description `r lifecycle::badge("stable")`
#' This internal function obtains the contact information for an
#' eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. It is the DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the contact information, such as: site manager, operation
#' organization, metadata provider, founding agency and site url.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @importFrom units set_units
#' @keywords internal
#'
### function get_site_contact
get_site_contact <- function(deimsid) {
  qo <- queries_jq_deims[[get_deims_API_version()]]$site_contact
  qo <- queries_jq_deims[[get_deims_API_version()]]$site_contact
  contact <- .materialise_query(qo, deimsid, "site_contact")
  
  if (is.null(contact) || nrow(contact) == 0L) {
    warning("No results returned for: ", deimsid)
    return(invisible(NULL))
  }
  
  contact$country <- unlist(contact$country)
  elev_cols <- c("geoElev.avg", "geoElev.min", "geoElev.max")
  contact[elev_cols] <- lapply(contact[elev_cols], units::set_units, value = "m")
  
  contact
}
