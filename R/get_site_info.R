#' @title eLTER get_site_info function
#' @description This function obtains details about an eLTER site
#' through the DEIMS-SDR sites API.
#' @param deimsid a character. The DEIMS ID of the site from
#' DEIMS-SDR website. More information about DEIMS ID in this pages:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @param category a `category`. This parameter selects which category
#' or categories are retrieved and returned in the result.
#' Possible value are:
#' 'Affiliations', 'Boundaries', 'Contacts', 'EnvCharacts', 'General',
#' 'Infrastructure', 'Parameters', 'RelateRes', 'ResearchTop'.
#' Multiple values can be indicated.
#' @param show_map a `boolean`. When TRUE, and category is "Boundaries" 
#' the boundary will be plotted on a Leaflet map. Default FALSE.
#' @return The output of the function is a `tibble` with main features of the
#' site and the selected information, such as: networks and projects in
#' which the site is involved.
#' If `category` "Boundaries" is indicated an `sf` object is returned 
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr GET content
#' @importFrom jqr jq
#' @importFrom jsonlite fromJSON stream_in
#' @importFrom dtplyr lazy_dt
#' @importFrom dplyr as_tibble left_join
#' @importFrom utils capture.output
#' @importFrom stringr str_detect
#' @importFrom leaflet leaflet addTiles addPolygons
#' @export
#' @examples
#' siteInfo <- get_site_info(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
#'   category = c("EnvCharacts", "Affiliations", "Boundaries")
#' )
#' siteInfo
#'
#' site <- get_site_info(
#'   deimsid = "https://deims.org/79d6c1df-570f-455f-a929-6cfe5c4ca1e9",
#'   category = "Boundaries"
#' )
#'
### function get_site_info
get_site_info <- function(deimsid, category = NA, show_map = FALSE) {
  q <- '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation
      }'
  url <- paste0(
    "https://deims.org/",
    "api/sites/",
    sub("^.+/", "", deimsid)
  )
  export <- httr::GET(url = url)
  jj <- suppressMessages(httr::content(export, "text"))
  status <- jj %>%
    jqr::jq(as.character("{status: .errors.status}")) %>%
    textConnection() %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE) %>%
    dtplyr::lazy_dt() %>%
    dplyr::as_tibble()
  if (is.na(status)) {
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
      if (any(stringr::str_detect(category, "Affiliations"))) {
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
      if (any(stringr::str_detect(category, "Contacts"))) {
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
      if (any(stringr::str_detect(category, "EnvCharacts"))) {
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
      if (any(stringr::str_detect(category, "General"))) {
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
      if (any(stringr::str_detect(category, "Infrastructure"))) {
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
      if (any(stringr::str_detect(category, "Parameters"))) {
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
      if (any(stringr::str_detect(category, "RelateRes"))) {
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
      if (any(stringr::str_detect(category, "ResearchTop"))) {
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
      if (any(stringr::str_detect(category, "Boundaries"))) {
        siteBound <- get_site_boundaries(deimsid = deimsid)
        siteInfo <- dplyr::left_join(
          siteBound,
          siteInfo,
          by = c(
            "title" = "title",
            "uri" = "uri"
          )
        )
        if (show_map == TRUE) {
          map <- leaflet::leaflet(siteInfo) %>%
            leaflet::addTiles() %>%
            leaflet::addPolygons()
          print(map)
        }
        return(siteInfo)
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
