#' Produce a pie chart of the parameters collected in a site LTER.
#' @description Return a pie chart of Environmental Parameters, as a stored in
#' \href{https://deims.org/}{DEIMS-SDR catalogue}, of a single eLTER site.
#' @param deimsid A `character`. It is the DEIMS ID of site/network from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a pie chart and a `tibble`. The
#' percentages, as a label in the pie charts and in the output table (
#' column 'perc'), refer to the number of the parameters, belonging to a type
#' (e.g. biological, atmospheric, etc.), measured compared to all of parameters
#' measured into selected eLTER site. This function allows to show what type
#' of parameters are most measured into a site. In the example below the
#' atmospheric parameters corresponds to the 15 percent of all parameters
#' measured into the site.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by tally mutate filter lag
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 theme_minimal theme element_blank element_text ggplot
#' @importFrom ggplot2 geom_text aes coord_fixed scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous scale_color_manual labs
#' @importFrom ggforce geom_arc_bar
#' @export
#' @examples
#' \dontrun{
#' pie <- produce_site_parameters_pie(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' )
#' pie
#' }
#'
#' @section The function output:
#' \figure{produce_site_parameters_pie_fig.png}{Parameters pie chart}
#'
### function produce_site_parameters_pie
produce_site_parameters_pie <- function(deimsid) {
  # TODO add this by SPARQL query
  site <- ReLTER::get_site_info(
    deimsid = deimsid,
    category = "Parameters"
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
    # parameters ----
    params <- params %>%
      dplyr::group_by(parameterGroups) %>%
      dplyr::tally() %>%
      dplyr::mutate(
        freq = n / sum(n),
        perc = paste0(round(freq, 2) * 100, "%"),
        end = 2 * pi * cumsum(freq) / sum(freq),
        start = dplyr::lag(end, default = 0),
        middle = 0.5 * (start + end),
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
      ggforce::geom_arc_bar(
        ggplot2::aes(
          x0 = 0,
          y0 = 0,
          r0 = 0,
          r = 1,
          start = start,
          end = end,
          fill = parameterGroups
        )
      ) +
      ggplot2::labs(
        title =
          paste0(
            "Percentage of parameters type measured in the ",
            site$title
          )
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          x = 1.05 * sin(middle),
          y = 1.05 * cos(middle),
          label = perc,
          hjust = hjust,
          vjust = vjust
        )
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
    params %>%
      dplyr::select(parameterGroups, n, freq, perc)
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS ID\n----\n")
    pie <- NULL
    params <- NULL
  }
}
