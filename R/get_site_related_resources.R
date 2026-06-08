#' eLTER get_site_related_resources function
#' @description `r lifecycle::badge("stable")`
#' This internal function obtains a list of related resources
#' collected in an eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of the
#' site and a list of the related resources collected by site.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @importFrom units set_units
#' @keywords internal
#'
### function get_site_related_resources
get_site_related_resources <- function(deimsid) {
  qo <- queries_jq_deims[[get_deims_API_version()]]$site_relatedResources
  relatedResources <- .materialise_query(qo, deimsid, "site_relatedResources")
  
  if (is.null(relatedResources) || nrow(relatedResources) == 0L) {
    warning("No results returned for: ", deimsid)
    return(invisible(NULL))
  }
  
  # Process or initialise relatedResources nested column
  if (!is.na(relatedResources$relatedResources)) {
    colnames(relatedResources$relatedResources[[1]]) <- c(
      "relatedResourcesId",
      "relatedResourcesTitle",
      "relatedResourcesChanged"
    )
    
    relatedResources$relatedResources[[1]] <- relatedResources$relatedResources[[1]] |>
      dplyr::mutate(
        uri = paste0(relatedResourcesId$prefix, relatedResourcesId$suffix),
        relatedResourcesChanged = as.POSIXct(relatedResourcesChanged, format = "%Y-%m-%dT%H:%M")
      ) |>
      dplyr::select("relatedResourcesTitle", "relatedResourcesChanged", "uri")
    
  } else {
    relatedResources$relatedResources <- list(
      data.frame(
        relatedResourcesId      = NA_character_,
        relatedResourcesTitle   = NA_character_,
        relatedResourcesChanged = as.POSIXct(NA)
      )
    )
  }
  
  # Flatten country from list-column to vector
  relatedResources$country <- unlist(relatedResources$country)
  
  # Set elevation units [m]
  elev_cols <- c("geoElev.avg", "geoElev.min", "geoElev.max")
  relatedResources[elev_cols] <- lapply(relatedResources[elev_cols], units::set_units, value = "m")
  
  relatedResources
}
