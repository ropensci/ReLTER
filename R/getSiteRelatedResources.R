#' @title eLTER_getSiteRelatedResources
#' @description This function allows to obtain the list of related resources collected in the eLTER site througth the DEIMS-SDR sites API.
#' @param deimsid
#' @return The output of the function is a tibble with main features of the site and the related resources collected by site.
#' @author Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
#' @import tibble httr
#' @export
#' @examples
#' getSiteRelatedResources(deimsid = 'https://deims.org/ede67a31-079a-4db5-b3a2-83b22054c661')
#'
### function getSiteRelatedResources
getSiteRelatedResources <- function(deimsid) {
  q = '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       relatedResources: .attributes.relatedResources
      }'
  url <- paste0("https://deims.org/", "api/sites/", substring(deimsid, 19))
  export <- httr::GET(url = url)
  jj <- httr::content(export, "text")
  relatedResources <- tibble::as_tibble(do_Q(q, jj))
  if (!is.na(relatedResources$relatedResources)) {
    colnames(relatedResources$relatedResources[[1]]) <- c("relatedResourcesId", "relatedResourcesTitle", "relatedResourcesChanged")
  } else {
    relatedResourcesId <- NA
    relatedResourcesTitle <- NA
    relatedResourcesChanged <- NA
    relatedResources$relatedResources <- list(data.frame(relatedResourcesId, relatedResourcesTitle, relatedResourcesChanged))
  }
  relatedResources
}

