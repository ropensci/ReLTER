#' eLTER get_site_envcharacts function
#' @description `r lifecycle::badge("stable")`
#' This internal function obtains Environmental Characteristics
#' of an eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. The DEIMS ID of a site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the environmental characteristics where available,
#' such as: air temperature, precipitation, biogeographical region, biome,
#' ecosystem land use, EUNIS habitat, geoBon biome, geology, hydrology, soils
#' and vegetation.
#' @author Alessandro Oggioni, phD (2021) \email{oggioni.a@irea.cnr.it}
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @importFrom units set_units
#' @keywords internal
#'
### function get_site_envcharacts
get_site_envcharacts <- function(deimsid) {
  qo <- queries_jq_deims[[get_deims_API_version()]]$site_envCharacts
  envCharacteristics <- .materialise_query(qo, deimsid, "site_envCharacts")
  
  if (is.null(envCharacteristics) || nrow(envCharacteristics) == 0L) {
    warning("No results returned for: ", deimsid)
    return(invisible(NULL))
  }
  
  # Flatten country from list-column to vector
  envCharacteristics$country <- unlist(envCharacteristics$country)
  
  # Set elevation units [m]
  elev_cols <- c("geoElev.avg", "geoElev.min", "geoElev.max")
  envCharacteristics[elev_cols] <- lapply(envCharacteristics[elev_cols], units::set_units, value = "m")
  
  # Set air temperature units [°C] — scalar column
  envCharacteristics$airTemperature.yearlyAverage <- units::set_units(
    envCharacteristics$airTemperature.yearlyAverage, "degree_Celsius"
  )
  
  # Set air temperature units [°C] — nested list-column (TODO resolved)
  envCharacteristics$airTemperature.monthlyAverage <- lapply(
    envCharacteristics$airTemperature.monthlyAverage,
    function(x) if (!is.null(x)) units::set_units(as.numeric(x), "degree_Celsius") else NA_real_
  )
  
  # Set air temperature reference period — coerce to NA if missing
  envCharacteristics$airTemperature.referencePeriod <- if (
    is.null(envCharacteristics$airTemperature.referencePeriod) ||
    all(is.na(envCharacteristics$airTemperature.referencePeriod))
  ) {
    units::set_units(NA_real_, "degree_Celsius")
  } else {
    units::set_units(envCharacteristics$airTemperature.referencePeriod, "degree_Celsius")
  }
  
  # Set precipitation units [mm] — scalar column
  envCharacteristics$precipitation.yearlyAverage <- units::set_units(
    envCharacteristics$precipitation.yearlyAverage, "mm"
  )
  
  # Set precipitation units [mm] — nested list-column (TODO resolved)
  envCharacteristics$precipitation.monthlyAverage <- lapply(
    envCharacteristics$precipitation.monthlyAverage,
    function(x) if (!is.null(x)) units::set_units(as.numeric(x), "mm") else NA_real_
  )
  
  # Set precipitation reference period — coerce to NA if missing
  envCharacteristics$precipitation.referencePeriod <- if (
    is.null(envCharacteristics$precipitation.referencePeriod) ||
    all(is.na(envCharacteristics$precipitation.referencePeriod))
  ) {
    units::set_units(NA_real_, "mm")
  } else {
    units::set_units(envCharacteristics$precipitation.referencePeriod, "mm")
  }
  
  envCharacteristics
}
