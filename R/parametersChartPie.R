#' @title eLTER parametersChartPie function
#' @description This function allows to obtain a pie chart of the parameter collected in a site or network grouped into compounds.
#' @param deimsid A `character`. It is the DEIMS iD of network make from DEIMS-SDR website. More information about DEIMS iD in this \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a pie chart.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import dplyr graphics data.table tibble scales grDevices RColorBrewer
#' @export 
#' @examples
#' pie <- parametersChartPie(deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe')
#' print(pie)
#'
### function parametersChartPie
parametersChartPie <- function(deimsid) {
  # TODO add this by SPARQL query
  data(envThesParams)
  
  paramsDeims <- ReLTER::getSiteParameters(deimsid)
  paramsDeims <- tibble::as_tibble(paramsDeims$parameter[[1]])
  params <- tibble::as_tibble(paramsDeims)
  params$parameterGroups <- parametersStructureEnvThes$category[match(params$parameterLabel, parametersStructureEnvThes$parameter)]
  
  groupsIsNa <- params %>% dplyr::filter(is.na(parameterGroups))
  
  # plot of parameters
  params <- params %>% 
    dplyr::group_by(parameterGroups) %>% 
    dplyr::tally() %>%
    dplyr::mutate(freq = n / sum(n))
  params$label <- scales::percent(params$freq)
  obsPropWaffle <- params$n
  names(obsPropWaffle) <- params$parameterGroups
  
  # Pie Graph
  pieGraphPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(nrow(params))
  graphics::pie(
    params$n,
    labels = paste0(params$parameterGroups, ' ', params$label),
    border = "white",
    col = pieGraphPalette,
    edges = 200,
    radius = 0.8,
    clockwise = TRUE,
    # IMPORTANT
    angle = 45
  ) 
  
  # warning about the Insufficient values in manual scale and 
  if (length(groupsIsNa$parameterLabel) == 0) {
    message('')
  } else {
    message(
      'This parameters are not included, please contact the development of the package by GitHub.\n',
      'Paste this message into the GitHub issue.\n',
      'I am using the parametersChart function and need to add the following parameters in the mapping:\n',
      paste(groupsIsNa$parameterLabel, collapse = '\n')
    )
  }
}


