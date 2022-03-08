#' @title Return a tibble object containing information, as a stored in
#' \href{https://deims.org/}{DEIMS-SDR catalogue}, of a single eLTER site.
#' @description This function obtains details about an eLTER site
#' through the DEIMS-SDR sites API.
#' @param deimsid a character. The DEIMS.iD of the site from
#' DEIMS-SDR website. DEIMS.iD information 
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @param category a `category`. This parameter selects which category
#' or categories are retrieved and returned in the result.
#' Possible value are:
#' 'Affiliations', 'Boundaries', 'Contacts', 'EnvCharacts', 'General',
#' 'Infrastructure', 'Parameters', 'RelateRes', 'ResearchTop'.
#' Multiple values can be indicated.
#' @return The output of the function is a `tibble` with main features of the
#' site and the selected information, such as: networks and projects in
#' which the site is involved.
#' If category 'Boundaries' is indicated an `sf` object is returned
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr RETRY content
#' @importFrom jqr jq
#' @importFrom jsonlite fromJSON stream_in
#' @importFrom dplyr as_tibble left_join
#' @importFrom utils capture.output
#' @importFrom leaflet leaflet addTiles addPolygons
#' @export
#' @examples
#' siteInfo <- get_site_info(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
#'   category = c("EnvCharacts", "Affiliations")
#' )
#' siteInfo
#'
#' site <- get_site_info(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
#'   category = "Boundaries"
#' )
#' site
#'
### function get_site_info
get_site_info <- function(deimsid, category = NA) {
  q <- '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation
      }'
  jj <- get_id(deimsid, "sites")
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        siteInfo <- dplyr::as_tibble(
          do_Q(q, jj)
        )
      )
    )
    if (any(is.na(category))) {
      siteInfo <- siteInfo
    } else {
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
      } else {
        siteInfo <- siteInfo
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
      } else {
        siteInfo <- siteInfo
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
      } else {
        siteInfo <- siteInfo
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
      } else {
        siteInfo <- siteInfo
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
      } else {
        siteInfo <- siteInfo
      }
      # add 'Parameters' info
      if (any(grepl("Parameters", category))) {
        siteParam <- get_site_parameters(deimsid = deimsid)
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
      } else {
        siteInfo <- siteInfo
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
      } else {
        siteInfo <- siteInfo
      }
      # add 'ResearchTop' info
      if (any(grepl("ResearchTop", category))) {
        siteResea <- get_site_research_topics(deimsid = deimsid)
        siteInfo <- dplyr::left_join(
          siteInfo,
          siteResea,
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
      } else {
        siteInfo <- siteInfo
      }
      # add 'Boundaries' info
      if (any(grepl("Boundaries", category))) {
        siteBound <- get_site_boundaries(deimsid = deimsid)
        if (!is.null(siteBound)) {
          siteInfo <- dplyr::left_join(
            siteBound,
            siteInfo,
            by = c("title" = "title", "uri" = "uri")
          )
        } else {
          siteInfo <- siteInfo
        }
      } else {
        siteInfo <- siteInfo
      }
    }
  } else {
    message("\n---- The requested page could not be found.
            Please check the DEIMS.iD ----\n")
    siteInfo <- NULL
  }
  # Final result
  siteInfo
}
