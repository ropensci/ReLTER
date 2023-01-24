#' Enrich and certify a list of species names by
#' comparing with \href{https://www.gbif.org}{GBIF}.
#' @description `r lifecycle::badge("experimental")`
#' This function provide tibble object with all the columns of input table
#' of taxa plus new columns obtained from:
#' \href{https://data-blog.gbif.org/post/gbif-backbone-taxonomy/}{
#' GBIF Backbone Taxonomy}.
#' This function use the `rgbif::name_backbone_checklist` function for
#' interact with API backbone taxonomy of GBIF.
#' @param input A `tibble`. The table that contain the species
#' names list to be checked.
#' @param taxaColumn A `numeric`. The cardinal number of the column where
#' species list is. Default is `1`.
#' @param refine A `logical`. With this selection, the function allows to
#' refine the result(s) that match with more GBIF records. By a interactive
#' use of the terminal, the user can chose the result. Default is `FALSE`.
#' @return When the `refine` is `FALSE` the output of the function is a
#' `tibble` with the refinement of the input, else the function simply
#' returns the unchanged input.
#'
#' All the labels of the columns are the terms of
#' \href{https://dwc.tdwg.org/terms/#record-level}{Darwin Core terms}.
#' The columns labels are annotate with the link (URI) of the
#' \href{https://dwc.tdwg.org/terms/#record-level}{Darwin Core terms}
#' as attributes of the `tibble`.
#'
#' @author Alessandro Oggioni, phD (2021) \email{oggioni.a@@irea.cnr.it}
#' @importFrom rgbif name_backbone_checklist
#' @importFrom dplyr select add_row rename
#' @importFrom tibble tibble add_column 
#' @references
#'   \insertRef{gbifR}{ReLTER}
#'
#'   \insertRef{dplyrR}{ReLTER}
#'   
#'   \insertRef{tibbleR}{ReLTER}
#' @export
#' @examples
#' \dontrun{
#' macrophytes <- tibble::tibble(
#'   ID = c(1:18),
#'   species = c("Ceratophyllum demersum",
#'   "Lagarosiphon major",
#'   "Littorella uniflora",
#'   "Myriophyllum spicatum",
#'   "Myriophyllum verticillatum",
#'   "Najas marina",
#'   "Najas minor",
#'   "Nelumbo nucifera",
#'   "Nitella gracilis",
#'   "Nuphar lutea",
#'   "Nymphaea alba",
#'   "Nymphoides peltata",
#'   "Persicaria amphibia",
#'   "Potamogeton perfoliatus",
#'   "Potamogeton pusillus",
#'   "Ranunculus sp.",
#'   "Stuckenia pectinata",
#'   "Trapa natans")
#' )
#' table <- taxon_id_gbif(
#'   input = macrophytes,
#'   taxaColumn = 2,
#'   refine = FALSE
#' )
#' table
#'
#' # The annotated URIs of columns label are achieved by:
#' attributes(table)$uri
#' 
#' }
#'
### function taxon_id_gbif
taxon_id_gbif <- function(
    input,
    taxaColumn = 1,
    refine = FALSE
) {
  n_cols_input <- length(input)
  colnames(input)[taxaColumn] <- "originalNameUsage"
  cols_without_uri <- rep("", (n_cols_input - 1))
  URIs <- c(
    cols_without_uri,
    "http://rs.tdwg.org/dwc/terms/originalNameUsage",
    "http://rs.tdwg.org/dwc/terms/scientificNameID",
    "http://rs.tdwg.org/dwc/terms/scientificName",
    "http://rs.tdwg.org/dwc/terms/taxonRank",
    "http://rs.tdwg.org/dwc/terms/taxonomicStatus",
    "http://rs.tdwg.org/dwc/terms/kingdom",
    "http://rs.tdwg.org/dwc/terms/phylum",
    "http://rs.tdwg.org/dwc/terms/order",
    "http://rs.tdwg.org/dwc/terms/family",
    "http://rs.tdwg.org/dwc/terms/genus"
  )
  if (isFALSE(refine)) {
    response_no_refine <- rgbif::name_backbone_checklist(
      name_data = input[[taxaColumn]],
      verbose = FALSE
    )
    response <- response_no_refine %>%
      dplyr::select(
        usageKey,
        scientificName,
        canonicalName,
        rank,
        status,
        confidence,
        matchType,
        kingdom,
        phylum,
        order,
        family,
        genus,
        species
      )
    difference <- (
      length(input[[taxaColumn]]) - nrow(response)
    )
    if (difference != 0) {
      message(
        "\n----\nThe list of taxa you provided was ",
        length(input[[taxaColumn]]), ".\n",
        "The taxa that have been matched in the GBIF\n",
        "backbone taxonomic are ",
        nrow(response), ".\n\n",
        difference,
        " taxa are not matched.\n\n",
        "We suggest to run again the function with\n",
        "refine = TRUE.\n----\n"
      )
    }
    response <- input %>%
      tibble::add_column(.data = response, .after = n_cols_input) %>%
      dplyr::rename(
        scientificNameID = usageKey,
        taxonRank = rank,
        taxonomicStatus = status
      ) %>%
      dplyr::select(
        -c(confidence, matchType, species, canonicalName)
      )
    attr(x = response, which = "uri") <- URIs
    return(response)
  } else {
    response <- tibble::tibble(
      usageKey = as.integer(),
      scientificName = as.character(),
      canonicalName = as.character(),
      rank = as.character(),
      status = as.character(),
      confidence = as.integer(),
      matchType = as.character(),
      kingdom = as.character(),
      phylum = as.character(),
      order = as.character(),
      family = as.character(),
      genus = as.character(),
      species = as.character()
    )
    for (i in seq_len(length(input[[taxaColumn]]))) {
      taxaName <- input[[taxaColumn]][i]
      return_gbif <- rgbif:::name_backbone_checklist(
        name_data = taxaName,
        verbose = TRUE
      )
      cols <- c(
        kingdom = NA, phylum = NA, order = NA, family = NA, genus = NA, species = NA
      )
      return_gbif <- tibble::add_column(
        return_gbif,
        !!!cols[setdiff(names(cols), names(return_gbif))]
      )
      exact_n <- which(return_gbif$matchType == "EXACT")
      if (length(exact_n) == 0 || length(exact_n) > 1) {
        for_refine <- return_gbif %>%
          dplyr::select(
            scientificName, canonicalName, rank,
            status, confidence, matchType
          )
        a <- lapply(list(1:nrow(for_refine))[[1]],
          function(x) {
            paste0(x,
              ": ", " ",
              for_refine$scientificName[[x]], "\n    rank: ",
              for_refine$rank[[x]], "\n    status: ",
              for_refine$status[[x]], "\n    confidance: ",
              for_refine$confidence[[x]], "\n    match type: ",
              for_refine$matchType[[x]], "\n"
            )
          }
        )
        message(
          "\n----\nThis is the taxa name provided by you:\n",
          taxaName,
          "\nGBIF don't contain a unique records that match with this name.\
The GBIF records most similar are:\n\n",
          a
        )
        my.selection <- NULL
        choiceNumber <- NULL
        my.selection <-
          readline(prompt = "\n----\nPlease select the record that you think
most similar to the taxa name that you have provided.
Insert the number of record:")
        if (!is.null(choiceNumber)) {
          my.selection <- choiceNumber
        }
        for_output <- return_gbif[my.selection,] %>%
          dplyr::select(usageKey, scientificName, canonicalName, rank, status,
                        confidence, matchType, kingdom, phylum, order, family,
                        genus, species)
        response <- response %>%
          dplyr::add_row(for_output)
      } else if (length(exact_n) == 1) {
        for_output <- return_gbif[exact_n,] %>%
          dplyr::select(usageKey, scientificName, canonicalName, rank, status,
                        confidence, matchType, kingdom, phylum, order, family,
                        genus, species)
        response <- response %>%
          dplyr::add_row(for_output)
      }
    }
    response <- response %>%
      tibble::add_column(.data = input, .after = n_cols_input) %>%
      dplyr::rename(
        scientificNameID = usageKey,
        taxonRank = rank,
        taxonomicStatus = status
      ) %>%
      dplyr::select(
        -c(confidence, matchType, species, canonicalName)
      )
    attr(x = response, which = "uri") <- URIs
    return(response)
  }
}
