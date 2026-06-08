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
  general <- .materialise_query(qo, deimsid, "site_general")
  
  if (is.null(general) || nrow(general) == 0L) {
    warning("No results returned for: ", deimsid)
    return(invisible(NULL))
  }
  
  # Flatten country from list-column to vector
  general$country <- unlist(general$country)
  
  # Set elevation units [m]
  elev_cols <- c("geoElev.avg", "geoElev.min", "geoElev.max")
  general[elev_cols] <- lapply(general[elev_cols], units::set_units, value = "m")
  
  general
}

#' @keywords internal
#' @noRd
.materialise_query <- function(qo, deimsid, query_name) {
  jj <- get_id(deimsid, qo$path)
  
  if (!is.na(attr(jj, "status"))) {
    message("\n----\nThe requested page could not be found.",
            "\nPlease check the DEIMS ID: ", deimsid, "\n----\n")
    return(invisible(NULL))
  }
  
  tryCatch({
    raw <- do_Q(qo$query, jj)
    if (inherits(raw, "dtplyr_step")) raw <- dplyr::collect(raw)
    dplyr::as_tibble(raw)
  }, error = function(e) {
    warning("Error in ", query_name, " query for: ", deimsid, "\n  -> ", conditionMessage(e))
    NULL
  })
}