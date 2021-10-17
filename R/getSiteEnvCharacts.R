#' @title eLTER getSiteEnvCharacts function
#' @description This function allows to obtain the information about
#' environmental characteristics of the eLTER site througth the DEIMS-SDR
#' sites API.
#' @param deimsid A `character`. It is the DEIMS iD of network make from
#' DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the environmental characteristics information, such as: air
#' temperature, precipitation, biogeographical region, biome, ecosystem land
#' use, EUNIS habitat, geoBon biome, geology, hydrology, soils and vegetation.
#' @author Alessandro Oggioni, phD (2021) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr GET content
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' tSiteEnvCharacts <- getSiteEnvCharacts(
#'   deimsid = "https://deims.org/17210eba-d832-4759-89fa-9ff127cbdf6e"
#' )
#' tSiteEnvCharacts
#'
### function getSiteEnvCharacts
getSiteEnvCharacts <- function(deimsid) {
  q <- '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       envCharacteristics: .attributes.environmentalCharacteristics
      }'

  url <- paste0(
    "https://deims.org/",
    "api/sites/",
    sub("^.+/", "", deimsid)
  )
  export <- httr::GET(url = url)
  jj <- suppressMessages(httr::content(export, "text"))
  status <- jj %>% 
    jqr::jq(as.character('{status: .errors.status}')) %>% 
    textConnection() %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE) %>%
    dtplyr::lazy_dt() %>% 
    dplyr::as_tibble()
  if (is.na(status)) {
    invisible(
      utils::capture.output(
        envCharacteristics <- dplyr::as_tibble(
          ReLTER::do_Q(q, jj)
        )
      )
    )
  } else {
    message("\n---- The requested page could not be found. Please check again the DEIMS.iD ----\n")
    envCharacteristics <- NULL
  }
  envCharacteristics
}
