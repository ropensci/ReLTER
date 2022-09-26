#' eLTER get_site_related_resources function
#' @description `r lifecycle::badge("stable")`
#' This internal function obtains a list of related resources
#' collected in an eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of the
#' site and a list of the related resources collected by site.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr RETRY content
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @keywords internal
#'
### function get_site_related_resources
get_site_related_resources <- function(deimsid) {
  qo <- queries_jq[[get_deims_API_version()]]$site_relatedResources
  jj <- get_id(deimsid, qo$path)
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        relatedResources <- dplyr::as_tibble(do_Q(qo$query, jj))
      )
    )
    if (!is.na(relatedResources$relatedResources)) {
      colnames(relatedResources$relatedResources[[1]]) <- c(
        "relatedResourcesId",
        "relatedResourcesTitle",
        "relatedResourcesChanged"
      )
      relatedResources$relatedResources[[1]]$uri <- paste0(
        relatedResources$relatedResources[[1]]$relatedResourcesId$prefix,
        relatedResources$relatedResources[[1]]$relatedResourcesId$suffix
      )
      relatedResources$relatedResources[[1]] <-
        relatedResources$relatedResources[[1]] %>%
          dplyr::select(
            "relatedResourcesTitle",
            "relatedResourcesChanged",
            "uri"
          )
      relatedResources$relatedResources[[1]]$relatedResourcesChanged <-
        as.POSIXct(
        relatedResources$relatedResources[[1]]$relatedResourcesChanged,
        format = "%Y-%m-%dT%H:%M"
      )
      relatedResources
    } else {
      relatedResourcesId <- NA
      relatedResourcesTitle <- NA
      relatedResourcesChanged <- NA
      relatedResources$relatedResources <- list(
        data.frame(
          relatedResourcesId,
          relatedResourcesTitle,
          relatedResourcesChanged
        )
      )
      relatedResources
    }
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS ID\n----\n")
    relatedResources <- NULL
  }
}
