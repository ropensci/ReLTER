#' @title eLTER produce_site_parameters_waffle function
#' @description This function produces a waffle chart of the parameters
#' collected in a site or network grouped into compounds.
#' @param deimsid A `character`. The DEIMS ID of site/network from:
#' DEIMS-SDR website. More information about DEIMS ID from:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a waffle chart.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by tally mutate filter
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom waffle waffle
#' @export
#' @examples
#' \dontrun{
#' waffle <- produce_site_parameters_waffle(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' )
#' print(waffle)
#' }
#'
### function produce_site_parameters_waffle
produce_site_parameters_waffle <- function(deimsid) {
  # TODO add this by SPARQL query
  paramsDeims <- ReLTER::get_site_info(
    deimsid = deimsid,
    category = "Parameters"
  )
  paramsDeims <- tibble::as_tibble(paramsDeims$parameter[[1]])
  if (length(paramsDeims) != 0) {
    params <- tibble::as_tibble(paramsDeims)
    params$parameterGroups <- parametersStructureEnvThes$category[
      match(params$parameterLabel, parametersStructureEnvThes$parameter)
    ]
    groupsIsNa <- params %>% dplyr::filter(is.na(parameterGroups))
    # plot of parameters ----
    params <- params %>%
      dplyr::group_by(parameterGroups) %>%
      dplyr::tally() %>%
      dplyr::mutate(freq = n / sum(n))
    params$label <- paste0(round(params$freq, 2) * 100, "%")
    obsPropWaffle <- params$n
    names(obsPropWaffle) <- params$parameterGroups
    # Waffle chart ----
    mycolors <- c(
      RColorBrewer::brewer.pal(
        name = "Set1",
        n = 9
      ),
      RColorBrewer::brewer.pal(
        name = "Set2",
        n = 8
      ),
      RColorBrewer::brewer.pal(
        name = "Set3",
        n = 12
      )
    )
    waffle <- waffle::waffle(
      obsPropWaffle,
      rows = 8,
      size = 3,
      xlab = paste0(
        "1 square is 1 parameter. Total of ",
        sum(params$n),
        " parameters"
      ),
      keep = FALSE,
      colors = mycolors
    )
    # warning about the Insufficient values in manual scale
    if (length(groupsIsNa$parameterLabel) == 0) {
      message("")
    } else {
      message(
        "This parameters are not included, please contact the development of the
        package by GitHub.\n",
        "Paste this message into the GitHub issue.\n",
        "I am using the parametersChart function and need to add the following
        parameters in the mapping:\n",
        paste(groupsIsNa$parameterLabel, collapse = "\n")
      )
    }
    print(waffle)
    params
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS.iD\n----\n")
    waffle <- NULL
    params <- NULL
  }
}
