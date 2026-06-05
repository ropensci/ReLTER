#' Produce a waffle chart of the observed properties collected in an eLTER site
#' @description `r lifecycle::badge("stable")`
#' Returns a waffle chart of environmental observed properties, as stored in
#' \href{https://deims.org/}{DEIMS-SDR catalogue}, for a single eLTER site.
#' The chart is built with \pkg{ggplot2} and requires no additional packages.
#' Each square represents one observed property; squares of the same colour
#' belong to the same parameter group (e.g. biological, atmospheric, etc.).
#' @param deimsid A `character`. The DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The function prints a waffle chart as a side effect and returns
#' a `tibble` with four columns:
#' \itemize{
#'   \item \code{parameterGroups} `character`. Name of the parameter group.
#'   \item \code{n} `integer`. Number of observed properties in the group.
#'   \item \code{freq} `double`. Relative frequency of the group.
#'   \item \code{label} `character`. Percentage label for the group.
#' }
#' Returns `invisible(NULL)` if the DEIMS ID is invalid or the site has no
#' observed properties.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by tally mutate filter
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual coord_equal
#' @importFrom ggplot2 labs theme theme_minimal element_blank element_text
#' @seealso [RColorBrewer::brewer.pal()]
#' @seealso [get_site_info()]
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
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop(
      "\n----\nThe function 'produce_site_observedProperties_waffle()' requires the optional package 'RColorBrewer'.\n",
      "Please install it with: install.packages(\"RColorBrewer\")\n----\n"
    )
  }
  
  site <- get_site_info(
    deimsid    = deimsid,
    categories = "observedProperties"
  )
  
  if (is.null(site)) return(invisible(NULL))
  
  paramsDeims <- tibble::as_tibble(site$observedProperties[[1]])
  
  if (nrow(paramsDeims) == 0L) {
    message("\n----\nThe requested page could not be found.",
            "\nPlease check the DEIMS ID\n----\n")
    return(invisible(NULL))
  }
  
  # Map observed properties to parameter groups
  paramsDeims$parameterGroups <- paste0(
    parametersStructureEnvThes$category[
      match(paramsDeims$observedPropertiesLabel, parametersStructureEnvThes$parameter)
    ],
    "s"
  )
  
  # Warn about unmapped properties
  groupsIsNa <- dplyr::filter(paramsDeims, is.na(parameterGroups))
  if (nrow(groupsIsNa) > 0L) {
    message(
      "These observed properties are not included, please open a GitHub issue:\n",
      "I am using produce_site_observedProperties_waffle() and need to add:\n",
      paste(groupsIsNa$observedPropertiesLabel, collapse = "\n")
    )
  }
  
  # Summarise by group
  params <- paramsDeims |>
    dplyr::group_by(parameterGroups) |>
    dplyr::tally() |>
    dplyr::mutate(freq = n / sum(n))
  params$label <- paste0(round(params$freq, 2) * 100, "%")
  
  # --- Build waffle grid manually with ggplot2 ---
  n_total    <- sum(params$n)
  n_cols     <- ceiling(sqrt(n_total))
  n_rows     <- ceiling(n_total / n_cols)
  
  # Expand each group to individual squares
  squares <- rep(params$parameterGroups, times = params$n)
  # Pad to fill grid
  squares <- c(squares, rep(NA, n_rows * n_cols - length(squares)))
  
  waffle_df <- data.frame(
    x     = rep(seq_len(n_cols), times = n_rows),
    y     = rep(seq_len(n_rows), each  = n_cols),
    group = squares
  )
  waffle_df <- waffle_df[!is.na(waffle_df$group), ]
  
  # Colours
  brewer.pal_fx <- getExportedValue("RColorBrewer", "brewer.pal")
  mycolors <- c(
    brewer.pal_fx(name = "Set1", n = 9),
    brewer.pal_fx(name = "Set2", n = 8),
    brewer.pal_fx(name = "Set3", n = 12)
  )
  n_groups <- length(unique(params$parameterGroups))
  mycolors <- mycolors[seq_len(n_groups)]
  
  waffle_plot <- ggplot2::ggplot(
    waffle_df,
    ggplot2::aes(x = x, y = y, fill = group)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_manual(
      values = mycolors,
      name   = "Parameter group"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title   = paste0(
        n_total, " observed properties measured in ",
        site$title.x, "\n(DEIMS ID: ", site$uri, ")"
      ),
      x = "1 square = 1 observed property",
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text        = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank(),
      panel.grid       = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(hjust = 0.5, size = 12),
      legend.text      = ggplot2::element_text(size = 10)
    )
  
  print(waffle_plot)
  params
}
