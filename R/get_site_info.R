#' Obtain details about an eLTER site.
#' @description `r lifecycle::badge("stable")`
#' This function obtains information of a single eLTER site,
#' as a stored in \href{https://deims.org/}{DEIMS-SDR catalogue},
#' through the DEIMS-SDR API.
#' @param deimsid A character. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @param category A `category`. This parameter selects which category
#' or categories are retrieved and returned in the result.
#' Possible value are:
#' 'Affiliations', 'Boundaries', 'Contacts', 'EnvCharacts', 'General',
#' 'Infrastructure', 'observedProperties', 'RelateRes'.
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
#' site <- get_site_info(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
#'   category = "Boundaries"
#' )
#' site
#'
#' siteInfo <- get_site_info(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
#'   category = c("EnvCharacts", "Affiliations")
#' )
#' siteInfo
#'
### function get_site_info
get_site_info <- function(deimsid, category = NA) {
  qo <- queries_jq[[get_deims_API_version()]]$site_info
  jj <- get_id(deimsid, qo$path)
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        siteInfo <- dplyr::as_tibble(do_Q(qo$query, jj))
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
      # the section about research topics of the site in DEIMS-SDR API version
      # 1.1 has been removed
      #
      # if (any(grepl("ResearchTop", category))) {
      #   siteResea <- get_site_research_topics(deimsid = deimsid)
      #   siteInfo <- dplyr::left_join(
      #     siteInfo,
      #     siteResea,
      #     by = c(
      #       "title" = "title",
      #       "uri" = "uri",
      #       "geoCoord" = "geoCoord",
      #       "country" = "country",
      #       "geoElev.avg" = "geoElev.avg",
      #       "geoElev.min" = "geoElev.min",
      #       "geoElev.max" = "geoElev.max",
      #       "geoElev.unit" = "geoElev.unit"
      #     )
      #   )
      # } else {
      #   siteInfo <- siteInfo
      # }
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
            Please check the DEIMS ID ----\n")
    siteInfo <- NULL
  }
  # Final result
  siteInfo
}
