#' Enrich and certify a list of species names by
#' comparing with \href{https://www.marinespecies.org}{WoRMS}.
#' @description `r lifecycle::badge("stable")`
#' This function provide tibble object with all the columns of input table
#' of taxa plus new columns such as valid_name, valid_authority, valid_AphiaID,
#' status, synonyms, LSID, url, matchType, nOfWormsRecords, wormsRecords
#' obtained from Word Register of Marine Species
#' \href{http://www.marinespecies.org/rest/}{WoRMS rest API}.
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
#' 
#' Most of the labels of the columns are the terms of
#' \href{https://dwc.tdwg.org/terms/#record-level}{Darwin Core terms}.
#' The columns labels are annotate with the link (URI) of the
#' \href{https://dwc.tdwg.org/terms/#record-level}{Darwin Core terms}
#' as attributes of the `tibble`.
#' 
#' @author Alessandro Oggioni, phD (2021) \email{oggioni.a@@irea.cnr.it}
#' @author Paolo Tagliolato, phD (2021) \email{tagliolato.p@@irea.cnr.it}
#' @importFrom worrms wm_records_names
#' @importFrom dplyr filter
#' @importFrom Rdpack reprompt
#' @references
#'   \insertRef{worrmsR}{ReLTER}
#'
#'   \insertRef{dplyrR}{ReLTER}
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
#' # The annotated URIs of columns label are achieved by:
#' attributes(table)$uri
#'
### function taxon_id_worms
taxon_id_worms <- function(
  input,
  taxaColumn = 1,
  verbose = TRUE,
  refine = FALSE
) {
  n_cols_input <- length(input)
  cols_without_uri <- rep("", (n_cols_input - 1))
  URIs <- c(
    cols_without_uri,
    "http://rs.tdwg.org/dwc/terms/originalNameUsage",
    "http://rs.tdwg.org/dwc/terms/scientificName",
    "http://rs.tdwg.org/dwc/terms/scientificNameAuthorship",
    "http://rs.tdwg.org/dwc/terms/taxonID",
    "http://rs.tdwg.org/dwc/terms/taxonomicStatus",
    "",
    "http://rs.tdwg.org/dwc/terms/taxonRank",
    "http://rs.tdwg.org/dwc/terms/kingdom",
    "http://rs.tdwg.org/dwc/terms/phylum",
    "http://rs.tdwg.org/dwc/terms/class",
    "http://rs.tdwg.org/dwc/terms/order",
    "http://rs.tdwg.org/dwc/terms/family",
    "http://rs.tdwg.org/dwc/terms/genus",
    "http://rs.tdwg.org/dwc/terms/scientificNameID",
    "",
    ""
  )
  input[, c(
    "valid_name", # ok
    "valid_authority", # ok
    "valid_AphiaID", # ok
    "status", # ok
    "synonyms", # ok yes or no
    "rank",
    "kingdom",
    "phylum",
    "class",
    "order",
    "family",
    "genus",
    "LSID", # ok
    "url", # ok
    "matchType",
    "nOfWormsResults",
    "wormsRecords"
  )] <- NA
  a <- list()
  for (m in seq_len(length(input[[taxaColumn]]))) {
    new_element <- tryCatch(
      worrms::wm_records_names(
      name = input[[taxaColumn]][m],
      marine_only = FALSE
    ), error=function(err) NA)
    a[[length(a) + 1]] <- new_element
  }
  for (i in seq_len(length(a))) {
    if (is.na(a[[i]])) {
      input$valid_name[[i]] <- NA
      input$valid_authority[[i]] <- NA
      input$valid_AphiaID[[i]] <- NA
      input$status[[i]] <- NA
      input$synonyms[[i]] <- NA
      input$rank[[i]] <- NA
      input$kingdom[[i]] <- NA
      input$phylum[[i]] <- NA
      input$class[[i]] <- NA
      input$order[[i]] <- NA
      input$family[[i]] <- NA
      input$genus[[i]] <- NA
      input$LSID[[i]] <- NA
      input$url[[i]] <- NA
      input$matchType[[i]] <- NA
      input$nOfWormsResults[[i]] <- 0
      input$wormsRecords[[i]] <- NA
      input$wormsRecords[[i]] <- NA
    } else if (nrow(a[[i]][[1]]) == 0) {
      input$nOfWormsResults[[i]] <- 0
      input$valid_name[[i]] <- NA
    } else if (nrow(a[[i]][[1]]) == 1) {
      input$valid_name[[i]] <- a[[i]][[1]]$valid_name
      input$valid_authority[[i]] <- a[[i]][[1]]$valid_authority
      input$valid_AphiaID[[i]] <- a[[i]][[1]]$valid_AphiaID
      input$status[[i]] <- a[[i]][[1]]$status
      input$synonyms[[i]] <- a[[i]][[1]]$unacceptreason
      input$rank[[i]] <- a[[i]][[1]]$rank
      input$kingdom[[i]] <- a[[i]][[1]]$kingdom
      input$phylum[[i]] <- a[[i]][[1]]$phylum
      input$class[[i]] <- a[[i]][[1]]$class
      input$order[[i]] <- a[[i]][[1]]$order
      input$family[[i]] <- a[[i]][[1]]$family
      input$genus[[i]] <- a[[i]][[1]]$genus
      input$LSID[[i]] <- a[[i]][[1]]$lsid
      input$url[[i]] <- a[[i]][[1]]$url
      input$matchType[[i]] <- a[[i]][[1]]$match_type
      input$nOfWormsResults[[i]] <- 1
      input$wormsRecords[[i]] <- list(a[[i]][[1]])
      input$wormsRecords[[i]] <- tibble::tibble(a[[i]][[1]])
    } else if (nrow(a[[i]][[1]]) > 1) {
      input$nOfWormsResults[[i]] <- nrow(a[[i]][[1]])
      input$wormsRecords[[i]] <- list(a[[i]][[1]])
      input$wormsRecords[[i]] <- tibble::tibble(a[[i]][[1]])
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
      colnames(refinedTable)[taxaColumn] <- "originalNameUsage"
      refinedTable <- refinedTable %>%
        dplyr::rename(
          scientificName = valid_name,
          scientificNameAuthorship = valid_authority,
          taxonID = valid_AphiaID,
          taxonomicStatus = status,
          taxonRank = rank,
          scientificNameID = LSID
        ) %>%
        dplyr::select(
          -c(url, matchType)
        )
      attr(x = refinedTable, which = "uri") <- URIs
      return(refinedTable)
    } else {
      colnames(newTable)[taxaColumn] <- "originalNameUsage"
      newTable <- newTable %>%
        dplyr::rename(
          scientificName = valid_name,
          scientificNameAuthorship = valid_authority,
          taxonID = valid_AphiaID,
          taxonomicStatus = status,
          taxonRank = rank,
          scientificNameID = LSID
        ) %>%
        dplyr::select(
          -c(url, matchType)
        )
      attr(x = newTable, which = "uri") <- URIs
      return(newTable)
    }
  } else {
    if (refine == TRUE) {
      refinedTable <- taxon_id_worms_refine(
        input = newTable,
        taxaColumn = taxaColumn,
        interaction = TRUE
      )
      colnames(refinedTable)[taxaColumn] <- "originalNameUsage"
      refinedTable <- refinedTable %>%
        dplyr::rename(
          scientificName = valid_name,
          scientificNameAuthorship = valid_authority,
          taxonID = valid_AphiaID,
          taxonomicStatus = status,
          taxonRank = rank,
          scientificNameID = LSID
        ) %>%
        dplyr::select(
          -c(url, matchType)
        )
      attr(x = refinedTable, which = "uri") <- URIs
      return(refinedTable)
    } else {
      colnames(newTable)[taxaColumn] <- "originalNameUsage"
      newTable <- newTable %>%
        dplyr::rename(
          scientificName = valid_name,
          scientificNameAuthorship = valid_authority,
          taxonID = valid_AphiaID,
          taxonomicStatus = status,
          taxonRank = rank,
          scientificNameID = LSID
        ) %>%
        dplyr::select(
          -c(url, matchType)
        )
      attr(x = newTable, which = "uri") <- URIs
      return(newTable)
    }
  }
}
