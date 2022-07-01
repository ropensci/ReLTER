#' Produce a waffle chart of the observed properties collected in a site LTER.
#' @description `r lifecycle::badge("stable")`
#' Return a waffle chart of Environmental observed properties, as a
#' stored in \href{https://deims.org/}{DEIMS-SDR catalogue}, of a single
#' eLTER site.
#' @param deimsid A `character`. The DEIMS ID of site/network from:
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a waffle chart and a `tibble`. Each
#' of the squares represents a observed properties measured into the selected
#' eLTER site. The observed properties with the same color belong to the same
#' group (e.g. biological, atmospheric, etc.).
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by tally mutate filter
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom waffle waffle
#' @export
#' @examples
#' \dontrun{
#' waffle <- produce_site_observedProperties_waffle(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' )
#' waffle
#' }
#'
#' @section The function output:
#' \figure{produce_site_parameters_waffle_fig.png}{Observed properties
#' waffle chart}
#'
### function produce_site_observedProperties_waffle
produce_site_observedProperties_waffle <- function(deimsid) {
  # TODO add this by SPARQL query
  site <- ReLTER::get_site_info(
    deimsid = deimsid,
    category = "observedProperties"
  )
  paramsDeims <- tibble::as_tibble(site$parameter[[1]])
  if (length(paramsDeims) != 0) {
    params <- tibble::as_tibble(paramsDeims)
    params$parameterGroups <- paste0(
      parametersStructureEnvThes$category[
        match(params$parameterLabel, parametersStructureEnvThes$parameter)
      ],
      "s"
    )
    groupsIsNa <- params %>% dplyr::filter(is.na(parameterGroups))
    # plot of observed properties ----
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
      title = paste0(
        "Observed properties measured in the ",
        site$title,
        " grouped by type"
      ),
      rows = 8,
      size = 3,
      xlab = paste0(
        "1 square is 1 observed property. A total of ",
        sum(params$n),
        " observed properties are collected in the ",
        site$title, " site (DEIMS ID: ",
        site$uri, ")"
      ),
      keep = FALSE,
      colors = mycolors
    )
    # warning about the Insufficient values in manual scale
    if (length(groupsIsNa$parameterLabel) == 0) {
      message("")
    } else {
      message(
        "This observed properties are not included, please contact the
        development of thepackage by GitHub.\n",
        "Paste this message into the GitHub issue.\n",
        "I am using the produce_site_observedProperties_waffle() function
        and need to add the following observed properties in the mapping:\n",
        paste(groupsIsNa$parameterLabel, collapse = "\n")
      )
    }
    print(waffle)
    params
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS ID\n----\n")
    waffle <- NULL
    params <- NULL
  }
}
