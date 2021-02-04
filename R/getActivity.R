#' @title eLTER_getActivity
#' @description This function allows to obtain the info of activity provided in DEIMS-SDR.
#' @param activityid
#' @return The output of the function is a tibble with main features of the activity make in a site.
#' @author Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
#' @import tibble httr
#' @export
#' @examples
#' getDataset(activityid = 'https://deims.org/activity/8689b125-ee46-4d09-9e46-640f9c5c6eab')
#'
### function getActivity
getActivity <- function(activityid) {
  q = '{title: .title
      }'
  # TODO add this field:
  # boundaries: .attributes.geographic.boundaries,
  url <- paste0("https://deims.org/", "api/", substring(datasetid, 19))
  export <- httr::GET(url = url)
  jj <- httr::content(export, "text")
  activity <- tibble::as_tibble(do_Q(q, jj))
  # TODO add if loops in order to filling the null fields into activity tibble
  # if (!is.na(activity$xxxxxxxxx)) {
  #   xxxxx
  # } else {
  #   xxxxx
  # }
  activity
}

