#' @title eLTER_getSiteEnvCharacts
#' @description This function allows to obtain the information acout environmental characteristics of the eLTER site througth the DEIMS-SDR sites API.
#' @param deimsid
#' @return The output of the function is a tibble with main features of the site and the environmental characteristics information, such as: air temperature, precipitation, biogeographical region, biome, ecosystem land use, EUNIS habitat, geoBon biome, geology, hydrology, soils and vegetation.
#' @author Alessandro Oggioni, phD (2021) <oggioni.a@irea.cnr.it>
#' @import tibble httr
#' @export
#' @examples
#' getSiteEnvCharacts(deimsid = 'https://deims.org/17210eba-d832-4759-89fa-9ff127cbdf6e')
#'
### function getSiteEnvCharacts
getSiteEnvCharacts <- function(deimsid) {
  q = '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       envCharacteristics: .attributes.environmentalCharacteristics
      }'

  url <- paste0("https://deims.org/", "api/sites/", substring(deimsid, 19))
  export <- httr::GET(url = url)
  jj <- httr::content(export, "text")
  envCharacteristics <- tibble::as_tibble(do_Q(q, jj))
  envCharacteristics
}
