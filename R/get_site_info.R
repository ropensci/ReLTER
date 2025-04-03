#' Obtain details about an eLTER site.
#' @description `r lifecycle::badge("stable")`
#' This function obtains information of a single eLTER site,
#' as a stored in \href{https://deims.org/}{DEIMS-SDR catalogue},
#' through the DEIMS-SDR API.
#' @param category A `category`. This parameter selects which category
#' or categories are retrieved and returned in the result.
#' Possible value are:
#' 'Affiliations', 'Contacts', 'EnvCharacts', 'General',
#' 'Infrastructure', 'observedProperties', 'RelateRes'.
#' Multiple values can be indicated.
#' A site's boundary is always returned.
#' @inheritParams get_site_boundaries
#' @return The output of the function is a `list` with two elements:
#' \itemize{
#' \item \code{map} A Leaflet map with the location if requested with
#' `show_map`.
#' \item \code{data} A `data.frame` with the information about the location.
#' \item \code{locations} A `data.frame` with the information about the
#' locations, if requested with `with_locations`.
#' }
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @importFrom dplyr as_tibble left_join
#' @importFrom utils capture.output
#' @importFrom leaflet leaflet addTiles addPolygons
#' @importFrom Rdpack reprompt
#' @importFrom units set_units
#' @importFrom lubridate as_datetime
#' @references
#'   \insertRef{dplyrR}{ReLTER}
#'
#'   \insertRef{utilsR}{ReLTER}
#'
#'   \insertRef{sfR}{ReLTER}
#'
#'   \insertRef{leafletR}{ReLTER}
#' @export
#' @examples
#' site <- get_site_info(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
#'   category = c("EnvCharacts", "Affiliations"),
#'   show_map = TRUE,
#'   with_locations = TRUE
#' )
#' site
#' 
### function get_site_info
get_site_info <- function(
    deimsid,
    category = NA,
    show_map = FALSE,
    with_locations = FALSE
  ) {
  qo <- queries_jq_deims[[get_deims_API_version()]]$site_info
  jj <- get_id(deimsid, qo$path)
  res <- list(
    map = NULL,
    data = NULL,
    locations = NULL
  )
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        siteInfo <- dplyr::as_tibble(do_Q(qo$query, jj))
      )
    )
    # set country field as vector
    siteInfo$country <- unlist(siteInfo$country)
    # add UOM to geoElev.avg, geoElev.min, and geoElev.max
    siteInfo$geoElev.avg <- units::set_units(
      x = siteInfo$geoElev.avg,
      value = 'm'
    )
    siteInfo$geoElev.min <- units::set_units(
      x = siteInfo$geoElev.min,
      value = 'm'
    )
    siteInfo$geoElev.max <- units::set_units(
      x = siteInfo$geoElev.max,
      value = 'm'
    )
    # set created and changed fields as data and time
    siteInfo$created <- lubridate::as_datetime(
      siteInfo$created
    )
    siteInfo$changed <- lubridate::as_datetime(
      siteInfo$changed
    )
    if (any(!is.na(category))) {
      # add 'Affiliations' info
      if (any(grepl("Affiliations", category))) {
        siteAffil <- get_site_affiliations(deimsid = deimsid)
        siteInfo <- dplyr::left_join(
          siteInfo,
          siteAffil,
          by = c(
            "title" = "title",
            "uri" = "uri",
            "geoCoord" = "geoCoord",
            "country" = "country",
            "geoElev.avg" = "geoElev.avg",
            "geoElev.min" = "geoElev.min",
            "geoElev.max" = "geoElev.max",
            "geoElev.unit" = "geoElev.unit"
          )
        )
      }
      # add 'Contacts' info
      if (any(grepl("Contacts", category))) {
        siteConta <- get_site_contact(deimsid = deimsid)
        siteInfo <- dplyr::left_join(
          siteInfo,
          siteConta,
          by = c(
            "title" = "title",
            "uri" = "uri",
            "geoCoord" = "geoCoord",
            "country" = "country",
            "geoElev.avg" = "geoElev.avg",
            "geoElev.min" = "geoElev.min",
            "geoElev.max" = "geoElev.max",
            "geoElev.unit" = "geoElev.unit"
          )
        )
      }
      # add 'EnvCharacts' info
      if (any(grepl("EnvCharacts", category))) {
        siteEnvCh <- get_site_envcharacts(deimsid = deimsid)
        siteInfo <- dplyr::left_join(
          siteInfo,
          siteEnvCh,
          by = c(
            "title" = "title",
            "uri" = "uri",
            "geoCoord" = "geoCoord",
            "country" = "country",
            "geoElev.avg" = "geoElev.avg",
            "geoElev.min" = "geoElev.min",
            "geoElev.max" = "geoElev.max",
            "geoElev.unit" = "geoElev.unit"
          )
        )
      }
      # add 'General' info
      if (any(grepl("General", category))) {
        siteGener <- get_site_general(deimsid = deimsid)
        siteInfo <- dplyr::left_join(
          siteInfo,
          siteGener,
          by = c(
            "title" = "title",
            "uri" = "uri",
            "geoCoord" = "geoCoord",
            "country" = "country",
            "geoElev.avg" = "geoElev.avg",
            "geoElev.min" = "geoElev.min",
            "geoElev.max" = "geoElev.max",
            "geoElev.unit" = "geoElev.unit"
          )
        )
      }
      # add 'Infrastructure' info
      if (any(grepl("Infrastructure", category))) {
        siteInfra <- get_site_infrastructure(deimsid = deimsid)
        siteInfo <- dplyr::left_join(
          siteInfo,
          siteInfra,
          by = c(
            "title" = "title",
            "uri" = "uri",
            "geoCoord" = "geoCoord",
            "country" = "country",
            "geoElev.avg" = "geoElev.avg",
            "geoElev.min" = "geoElev.min",
            "geoElev.max" = "geoElev.max",
            "geoElev.unit" = "geoElev.unit"
          )
        )
      }
      # add 'Observed properties' info
      if (any(grepl("observedProperties", category))) {
        siteParam <- get_site_observedProperties(deimsid = deimsid)
        siteInfo <- dplyr::left_join(
          siteInfo,
          siteParam,
          by = c(
            "title" = "title",
            "uri" = "uri",
            "geoCoord" = "geoCoord",
            "country" = "country",
            "geoElev.avg" = "geoElev.avg",
            "geoElev.min" = "geoElev.min",
            "geoElev.max" = "geoElev.max",
            "geoElev.unit" = "geoElev.unit"
          )
        )
      }
      # add 'RelateRes' info
      if (any(grepl("RelateRes", category))) {
        siteRelat <- get_site_related_resources(deimsid = deimsid)
        siteInfo <- dplyr::left_join(
          siteInfo,
          siteRelat,
          by = c(
            "title" = "title",
            "uri" = "uri",
            "geoCoord" = "geoCoord",
            "country" = "country",
            "geoElev.avg" = "geoElev.avg",
            "geoElev.min" = "geoElev.min",
            "geoElev.max" = "geoElev.max",
            "geoElev.unit" = "geoElev.unit"
          )
        )
      }
    }
    if (show_map) {
      bound <- get_site_boundaries(
        deimsid = deimsid,
        show_map = TRUE,
        with_locations = with_locations
      )
      siteInfo <- siteInfo %>%
        dplyr::left_join(
          bound$data,
          by = c("uri" = "uri")
        ) %>%
         sf::st_as_sf(sf_column_name = "geometry")
      res$map <- bound$map
      res$locations <- bound$locations
    }
    res$data <- siteInfo
  } else {
    message("\n----\nThe requested page could not be found.
            Please check the DEIMS ID\n----\n")
  }
  # Final result
  return(res)
}
