#' Enrich and certify a list of species names by
#' comparing with \href{https://www.marinespecies.org}{Worms}.
#' @description `r lifecycle::badge("stable")`
#' This function tibble object with all the columns of input table
#' of taxa plus new columns such as valid_name, valid_authority, valid_AphiaID,
#' status, synonyms, LSID, url, matchType, nOfWormsRecords, wormsRecords
#' obtained from:
#' \href{http://www.marinespecies.org/rest/}{Worms rest API}.
#' @param input A `tibble`. The table that contain the species
#' names list to be checked.
#' @param taxaColumn A `numeric`. The cardinal number of the column where
#' species list is. Default is `1`.
#' @param verbose A `logical`. Whit this selection, the function returns a
#' message with number of record(s) that don't match with any Worms names and
#' the number of record(s) that match with more that one Worms name. Default
#' is `TRUE`.
#' @param refine A `logical`. With this selection, the function allows to
#' refine the result(s) that match with more Worms records. By a interactive
#' use of the terminal, the user can chose the result. Default is `FALSE`.
#' @return The output of the function is a `tibble` with the columns provided
#' and new columns such as: valid_name, valid_authority, valid_AphiaID, status,
#' synonyms, LSID, url, matchType, nOfWormsRecords, wormsRecords obtained by
#' \href{http://www.marinespecies.org/rest/}{Worms rest API}.
#' The function also return, if verbose is TRUE, the list of records that don't
#' match with Worms name species.
#' @author Alessandro Oggioni, phD (2021) \email{oggioni.a@@irea.cnr.it}
#' @author Paolo Tagliolato, phD (2021) \email{tagliolato.p@@irea.cnr.it}
#' @importFrom worrms wm_records_names
#' @importFrom dplyr filter
#' @export
#' @examples
#' phytoplankton <- tibble::tibble(
#'    ID = c(1, 2, 3, 4, 5, 6, 7),
#'    species = c(
#'    "Asterionella formosa", "Chrysococcus sp.",
#'    "Cryptomonas rostrata", "Dinobryon divergens",
#'    "Mallomonas akrokomos", "Melosira varians",
#'    "Cryptomonas rostrata"
#'  )
#' )
#' table <- taxon_id_worms(
#'   input = phytoplankton,
#'   taxaColumn = 2,
#'   verbose = TRUE,
#'   refine = TRUE
#' )
#' table
#'
### function taxon_id_worms
taxon_id_worms <- function(
  input,
  taxaColumn = 1,
  verbose = TRUE,
  refine = FALSE
) {
  input[, c(
    "valid_name", # ok
    "valid_authority", # ok
    "valid_AphiaID", # ok
    "status", # ok
    "synonyms", # ok yes or no
    "LSID", # ok
    "url", # ok
    "matchType",
    "nOfWormsResults",
    "wormsRecords"
  )] <- NA
  a <- worrms::wm_records_names(
    name = input[[taxaColumn]],
    marine_only = FALSE
  )
  for (i in seq_len(length(a))) {
    if (nrow(a[[i]]) == 0) {
      input$nOfWormsResults[[i]] <- 0
      input$valid_name[[i]] <- NA
    } else if (nrow(a[[i]]) == 1) {
      input$valid_name[[i]] <- a[[i]]$valid_name
      input$valid_authority[[i]] <- a[[i]]$valid_authority
      input$valid_AphiaID[[i]] <- a[[i]]$valid_AphiaID
      input$status[[i]] <- a[[i]]$status
      input$synonyms[[i]] <- a[[i]]$unacceptreason
      input$LSID[[i]] <- a[[i]]$lsid
      input$url[[i]] <- a[[i]]$url
      input$matchType[[i]] <- a[[i]]$match_type
      input$nOfWormsResults[[i]] <- 1
      input$wormsRecords[[i]] <- list(a[[i]])
      input$wormsRecords[[i]] <- tibble::tibble(a[[i]])
    } else if (nrow(a[[i]]) > 1) {
      input$nOfWormsResults[[i]] <- nrow(a[[i]])
      input$wormsRecords[[i]] <- list(a[[i]])
      input$wormsRecords[[i]] <- tibble::tibble(a[[i]])
    }
  }
  newTable <- input
  if (verbose == TRUE) {
    rowsWithZero <- newTable %>%
      dplyr::filter("nOfWormsResults" == 0) %>%
      nrow()
    rowsWithMoreOne <- newTable %>%
      dplyr::filter("nOfWormsResults" > 1) %>%
      nrow()
    message <- message(
      paste0(
        "\nAfter the finding operation the number of record(s) that don't match
with any Worms names are:\n*--- ",
        rowsWithZero, " on ", length(a), " examined",
        " ---*\nplease verify the species name provided and run again this
function. The record(s) that match with more that one Worms name are:\n*--- ",
        rowsWithMoreOne, " on ", length(a), " examined",
        " ---*\nplease use the function taxon_id_worms_refine for specify wich
is the exact corrispondence with your given species name.\n"
      )
    )
    if (refine == TRUE) {
      refinedTable <- taxon_id_worms_refine(
        input = newTable,
        taxaColumn = taxaColumn,
        interaction = TRUE
      )
      refinedTable
    } else {
      newTable
    }
  } else {
    if (refine == TRUE) {
      refinedTable <- taxon_id_worms_refine(
        input = newTable,
        taxaColumn = taxaColumn,
        interaction = TRUE
      )
      refinedTable
    } else {
      newTable
    }
  }
}
