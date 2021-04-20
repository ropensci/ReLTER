#' @title eLTER getActivity function
#' @description This function allows to obtain the info of activity (actually only title) provided in DEIMS-SDR.
#' @param activityid A character. It is the DEIMS iD of activity make from DEIMS-SDR website. More information about DEIMS iD in this \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a `tibble` with main features of the activity make in a site.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import tibble httr
#' @export
#' @examples
#' getActivity(activityid = 'https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845')
#'
### function getActivity
getActivity <- function(activityid) {
  q = '{title: .title
      }'
  # TODO add this field:
  # boundaries: .attributes.geographic.boundaries,
  url <- paste0("https://deims.org/", "api/activities/", substring(activityid, 28))
  export <- httr::GET(url = url)
  jj <- httr::content(export, "text")
  activity <- tibble::as_tibble(ReLTER::do_Q(q, jj))
  # TODO add if loops in order to filling the null fields into activity tibble
  # if (!is.na(activity$xxxxxxxxx)) {
  #   xxxxx
  # } else {
  #   xxxxx
  # }
  activity
}

