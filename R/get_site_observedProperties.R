#' eLTER get_site_observedProperties function
#' @description `r lifecycle::badge("stable")`
#' This internal function obtains the list of observed properties measured
#' in the eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. It is the DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the observed properties collected.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @importFrom units set_units
#' @keywords internal
#'
### function get_site_observedProperties
get_site_observedProperties <- function(deimsid) {
  qo <- queries_jq_deims[[get_deims_API_version()]]$site_observedProperties
  observedProperties <- .materialise_query(qo, deimsid, "site_observedProperties")
  
  if (is.null(observedProperties) || nrow(observedProperties) == 0L) {
    warning("No results returned for: ", deimsid)
    return(invisible(NULL))
  }
  
  # Rename or initialise observedProperties nested column
  if (!is.na(observedProperties$observedProperties)) {
    colnames(observedProperties$observedProperties[[1]]) <- c(
      "observedPropertiesLabel",
      "observedPropertiesUri"
    )
  } else {
    observedProperties$observedProperties <- list(
      data.frame(
        observedPropertiesLabel = character(0),
        observedPropertiesUri   = character(0)
      )
    )
  }
  
  # Flatten country from list-column to vector
  observedProperties$country <- unlist(observedProperties$country)
  
  # Set elevation units [m]
  elev_cols <- c("geoElev.avg", "geoElev.min", "geoElev.max")
  observedProperties[elev_cols] <- lapply(observedProperties[elev_cols], units::set_units, value = "m")
  
  observedProperties
}
