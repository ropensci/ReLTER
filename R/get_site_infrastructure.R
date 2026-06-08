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
  qo <- queries_jq_deims[[get_deims_API_version()]]$site_infrastructure
  infrastructure <- .materialise_query(qo, deimsid, "site_infrastructure")
  
  if (is.null(infrastructure) || nrow(infrastructure) == 0L) {
    warning("No results returned for: ", deimsid)
    return(invisible(NULL))
  }
  
  # Rename collection columns for clarity
  colnames(infrastructure$collection[[1]]) <- c(
    "collectionLabel",
    "collectionURI"
  )
  
  # Flatten country from list-column to vector
  infrastructure$country <- unlist(infrastructure$country)
  
  # Set elevation units [m]
  elev_cols <- c("geoElev.avg", "geoElev.min", "geoElev.max")
  infrastructure[elev_cols] <- lapply(infrastructure[elev_cols], units::set_units, value = "m")
  
  infrastructure
}
