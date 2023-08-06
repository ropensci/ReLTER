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
#' @author Alessandro Oggioni, phD (2021) \email{oggioni.a@@irea.cnr.it}
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @importFrom units set_units
#' @keywords internal
#'
### function get_site_envcharacts
get_site_envcharacts <- function(deimsid) {
  qo <- queries_jq[[get_deims_API_version()]]$site_envCharacts
  jj <- get_id(deimsid, qo$path)
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        envCharacteristics <- dplyr::as_tibble(do_Q(qo$query, jj))
      )
    )
    # set country field as vector
    envCharacteristics$country <- unlist(envCharacteristics$country)
    # set the UOM of geoElev.avg, geoElev.min, and geoElev.max
    envCharacteristics$geoElev.avg <- units::set_units(
      x = envCharacteristics$geoElev.avg,
      value = 'm'
    )
    envCharacteristics$geoElev.min <- units::set_units(
      x = envCharacteristics$geoElev.min,
      value = 'm'
    )
    envCharacteristics$geoElev.max <- units::set_units(
      x = envCharacteristics$geoElev.max,
      value = 'm'
    )
    # set the UOM of airTemperature info
    envCharacteristics$envCharacteristics.airTemperature.yearlyAverage <- units::set_units(
      x = envCharacteristics$envCharacteristics.airTemperature.yearlyAverage,
      value = '°C'
    )
    envCharacteristics$envCharacteristics.airTemperature.monthlyAverage <- units::set_units(
      x = envCharacteristics$envCharacteristics.airTemperature.monthlyAverage,
      value = '°C'
    )
    if (is.null(envCharacteristics$envCharacteristics.airTemperature.referencePeriod)) {
      envCharacteristics$envCharacteristics.airTemperature.referencePeriod <- NA
      envCharacteristics$envCharacteristics.airTemperature.referencePeriod <- units::set_units(
        x = envCharacteristics$envCharacteristics.airTemperature.referencePeriod,
        value = '°C'
      )
    }
    # set the UOM of precipitation info
    envCharacteristics$envCharacteristics.precipitation.yearlyAverage <- units::set_units(
      x = envCharacteristics$envCharacteristics.precipitation.yearlyAverage,
      value = 'mm'
    )
    envCharacteristics$envCharacteristics.precipitation.monthlyAverage <- units::set_units(
      x = envCharacteristics$envCharacteristics.precipitation.monthlyAverage,
      value = 'mm'
    )
    if (!is.null(envCharacteristics$envCharacteristics.precipitation.referencePeriod)) {
      envCharacteristics$envCharacteristics.precipitation.referencePeriod <- NA
      envCharacteristics$envCharacteristics.precipitation.referencePeriod <- units::set_units(
        x = envCharacteristics$envCharacteristics.precipitation.referencePeriod,
        value = 'mm'
      )
    }
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS ID\n----\n")
    envCharacteristics <- NULL
  }
  envCharacteristics
}
