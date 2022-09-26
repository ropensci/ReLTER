#' eLTER get_site_contact function
#' @description `r lifecycle::badge("stable")`
#' This internal function obtains the contact information for an
#' eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. It is the DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the contact information, such as: site manager, operation
#' organization, metadata provider, founding agency and site url.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom httr RETRY content
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @keywords internal
#'
### function get_site_contact
get_site_contact <- function(deimsid) {
  qo <- queries_jq[[get_deims_API_version()]]$site_contact
  jj <- get_id(deimsid, qo$path)
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        contact <- dplyr::as_tibble(do_Q(qo$query, jj))
      )
    )
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS ID\n----\n")
    contact <- NULL
  }
  contact
}
