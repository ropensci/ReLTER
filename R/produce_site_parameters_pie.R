#' @title eLTER produce_site_parameters_pie function
#' @description This function produces a pie chart of the parameters
#' collected in a site or network grouped into compounds.
#' @param deimsid A `character`. It is the DEIMS ID of site/network from
#' DEIMS-SDR website. More information about DEIMS ID from:
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a pie chart.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by tally mutate filter
#' @importFrom scales percent
#' @importFrom RColorBrewer brewer.pal
#' @importFrom utils data
#' @importFrom ggplot2 theme_minimal theme element_blank element_text ggplot
#' @importFrom ggplot2 geom_text aes coord_fixed scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous scale_color_manual
#' @importFrom ggforce geom_arc_bar
#' @importFrom stats lag start end
#' @export
#' @examples
#' \dontrun{
#' pie <- produce_site_parameters_pie(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' )
#' pie
#' }
#'
### function produce_site_parameters_pie
produce_site_parameters_pie <- function(deimsid) {
  # TODO add this by SPARQL query
  # parametersStructureEnvThes <- utils::data(envThesParams)
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
    groupsIsNa <- params %>% dplyr::filter(is.na("parameterGroups"))
    # parameters ----
    params <- params %>%
      dplyr::group_by("parameterGroups") %>%
      dplyr::tally() %>%
      dplyr::mutate(
        freq = n / sum(n),
        label = scales::percent(freq),
        end = 2 * pi * cumsum(freq) / sum(freq),
        start = stats::lag(end, default = 0),
        middle = 0.5 * (stats::start + stats::start),
        hjust = ifelse(middle > pi, 1, 0),
        vjust = ifelse(
          middle < pi / 2 | middle > 3 * pi / 2, 0, 1
          )
      )
    # Pie Graph ----
    blank_theme <- ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          size = 14, face = "bold"
        )
      )
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
    pie <- ggplot2::ggplot(params) +
      ggforce::geom_arc_bar(ggplot2::aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                       start = start, end = end, fill = parameterGroups)) +
      ggplot2::geom_text(
        ggplot2::aes(
          x = 1.05 * sin(middle),
          y = 1.05 * cos(middle),
          label = label,
          hjust = hjust,
          vjust = vjust)
        ) +
      ggplot2::coord_fixed() +
      ggplot2::scale_x_continuous(
        limits = c(-1.5, 1.5), # Adjust so labels are not cut off
        name = "",
        breaks = NULL,
        labels = NULL
      ) +
      ggplot2::scale_y_continuous(
        limits = c(-1, 1.1), # Adjust so labels are not cut off
        name = "",
        breaks = NULL,
        labels = NULL
      ) +
      blank_theme +
      ggplot2::scale_color_manual(
        values = mycolors
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
    print(pie)
    params
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS.iD\n----\n")
    pie <- NULL
    params <- NULL
  }
}
