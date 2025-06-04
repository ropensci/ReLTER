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
#' @importFrom ggplot2 theme ggtitle element_text
#' @seealso [waffle::waffle()]
#' @seealso [RColorBrewer::brewer.pal()]
#' @references
#'   \insertRef{tibbleR}{ReLTER}
#'
#'   \insertRef{dplyrR}{ReLTER}
#'   
#'   \insertRef{ggplot2R}{ReLTER}
#'
#'   \insertRef{waffleR}{ReLTER}
#'   
#'   \insertRef{RColorBrewerR}{ReLTER}
#' @export
#' @examples
#' \dontrun{
#' waffle <- produce_site_observedProperties_waffle(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' )
#' }
#'
#' @section The function output:
#' \figure{produce_site_parameters_waffle_fig.png}{Observed properties
#' waffle chart}
#'
### function produce_site_observedProperties_waffle
produce_site_observedProperties_waffle <- function(deimsid) {
  # Check if required packages are installed
  if (!requireNamespace("waffle", quietly = TRUE)) {
    stop(
      "\n----\nThe function 'produce_site_observedProperties_waffle()' requires the optional package 'waffle'.\n",
      "Please install it with: install.packages(\"waffle\")\n----\n"
    )
  }
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop(
      "\n----\nThe function 'get_site_speciesOccurrences()' requires the optional package 'RColorBrewer'.\n",
      "Please install it with: install.packages(\"RColorBrewer\")\n----\n"
    )
  }
  site <- get_site_info(
    deimsid = deimsid,
    categories = "observedProperties"
  )
  paramsDeims <- tibble::as_tibble(site$data$observedProperties[[1]])
  if (length(paramsDeims) != 0) {
    params <- tibble::as_tibble(paramsDeims)
    params$parameterGroups <- paste0(
      parametersStructureEnvThes$category[
        match(params$observedPropertiesLabel, parametersStructureEnvThes$parameter)
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
    brewer.pal_fx <- getExportedValue("RColorBrewer", "brewer.pal")
    mycolors <- c(
      brewer.pal_fx(
        name = "Set1",
        n = 9
      ),
      brewer.pal_fx(
        name = "Set2",
        n = 8
      ),
      brewer.pal_fx(
        name = "Set3",
        n = 12
      )
    )
    waffle_fx <- getExportedValue("waffle", "waffle")
    waffle <- waffle_fx(
      obsPropWaffle,
      title = paste0(
        "Observed properties measured in the ",
        site$data$title,
        " grouped by type"
      ),
      rows = 8,
      size = 3,
      xlab = "1 square is 1 observed property",
      keep = FALSE,
      colors = mycolors
    ) + ggplot2::ggtitle(
      paste0(
        sum(params$n),
        " observed properties measured in the ",
        site$data$title, "site\n(DEIMS ID: ",
        site$data$uri, ")"
      )
    ) + ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5, size = 12
        ),
        legend.text = ggplot2::element_text(size = 10)
      )
    # warning about the Insufficient values in manual scale
    if (length(groupsIsNa$observedPropertiesLabel) == 0) {
      message("")
    } else {
      message(
        "This observed properties are not included, please contact the
        development of thepackage by GitHub.\n",
        "Paste this message into the GitHub issue.\n",
        "I am using the produce_site_observedProperties_waffle() function
        and need to add the following observed properties in the mapping:\n",
        paste(groupsIsNa$observedPropertiesLabel, collapse = "\n")
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
