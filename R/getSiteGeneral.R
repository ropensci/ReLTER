#' @title eLTER_getSiteGeneral
#' @description This function allows to obtain the general info of the eLTER site througth the DEIMS-SDR sites API.
#' @param deimsid
#' @return The output of the function is a tibble with main features of the site and the general information, such as: abstract, keywords, porpose, status, yearEstablished, yearClosed, hierarchy, siteName, short name, site type, protection level, images.
#' @author Alessandro Oggioni, phD (2020) <oggioniale@gmail.com>
#' @import jsonlite
#' @export
#' @examples
#' getSiteGeneral(deimsid = 'https://deims.org/17210eba-d832-4759-89fa-9ff127cbdf6e')
#'
### function getSiteGeneral
getSiteGeneral <- function(deimsid) {
  q = '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       generalInfo: .attributes.general
      }'
  url <- paste0("https://deims.org/", "api/sites/", substring(deimsid, 19))
  export <- httr::GET(url = url)
  jj <- httr::content(export, "text")
  general <- tibble::as_tibble(do_Q(q, jj))
  colnames(general$generalInfo.keywords[[1]]) <- c('keywordsLabel', 'keywordsURI')
  general
}
