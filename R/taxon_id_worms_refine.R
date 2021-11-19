#' @title eLTER taxon_id_worms_refine
#' @description This function provides a refining of the output of
#' function taxon_id_worms.
#' @param input a `tibble`. The output of taxon_ID_Worms.
#' @param taxaColumn a `numeric`. The cardinal number of the column where
#' species list is. Default is `1`.
#' @return The output of the function is a `tibble` with the refinement of the
#' input.
#' @author Alessandro Oggioni, phD (2021) \email{oggioni.a@@irea.cnr.it}
#' @importFrom dplyr filter select
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' phytoplankton <- tibble::tibble(
#'    ID = c(1, 2, 3, 4, 5, 6, 7),
#'    species = c(
#'      "Asterionella formosa", "Chrysococcus sp.",
#'      "Cryptomonas rostrata", "Dinobryon divergens",
#'      "Mallomonas akrokomos", "Melosira varians",
#'      "Cryptomonas rostrata"
#'    )
#' )
#' table <- taxon_id_worms(
#'   input = phytoplankton,
#'   taxaColumn = 2,
#'   verbose = TRUE,
#'   refine = FALSE
#' )
#' table
#' taxon_id_worms_refine(input = table, taxaColumn = 2)
#' }
### function taxon_id_worms_refine
taxon_id_worms_refine <- function(input, taxaColumn = 1) {
  for (i in seq_len(nrow(input))) {
    if (input$nOfWormsResults[[i]] > 1) {
      taxaName <- input$species[[i]]
      a <- lapply(list(1:input$nOfWormsResults[[i]])[[1]],
                  function(x) {
                    paste0(
                      x,
                      ": ",
                      " ",
                      input$wormsRecords[[i]]$scientificname[[x]],
                      " (",
                      input$wormsRecords[[i]]$authority[[x]],
                      ")\n    Worms status: ",
                      input$wormsRecords[[i]]$status[[x]],
                      "\n    Unaccept reason: ",
                      input$wormsRecords[[i]]$unacceptreason[[x]],
                      "\n    Match type: ",
                      input$wormsRecords[[i]]$match_type[[x]],
                      "\n    Modified: ",
                      input$wormsRecords[[i]]$modified[[x]],
                      "\n"
                    )
                  })
      message(
        "This is the taxa name provided by you:\n",
        taxaName,
        "\nWorms don't contain a unique records that match with this name.\
The Worms records most similar are:\n\n",
        a
      )
      my.selection <-
        readline(prompt = "\n----\nPlease select the record that you think most
similar to the taxa name that you have provided. Insert the number of record: ")
      input$valid_name[[i]] <- input$wormsRecords[[i]][as.integer(my.selection), ]$valid_name
      input$valid_authority[[i]] <- input$wormsRecords[[i]][as.integer(my.selection), ]$valid_authority
      input$valid_AphiaID[[i]] <- input$wormsRecords[[i]][as.integer(my.selection), ]$valid_AphiaID
      input$status[[i]] <- input$wormsRecords[[i]][as.integer(my.selection), ]$status
      input$synonyms[[i]] <- input$wormsRecords[[i]][as.integer(my.selection), ]$unacceptreason
      input$LSID[[i]] <- input$wormsRecords[[i]][as.integer(my.selection), ]$lsid
      input$url[[i]] <- input$wormsRecords[[i]][as.integer(my.selection), ]$url
      input$matchType[[i]] <- input$wormsRecords[[i]][as.integer(my.selection), ]$match_type
      input$nOfWormsResults[[i]] <- 1
      input$wormsRecords[[i]] <- list(
        input$wormsRecords[[i]][as.integer(my.selection), ]
      )
      input$wormsRecords[[i]] <- tibble::tibble(input$wormsRecords[[i]][[1]])
    } else {
      input[i, ] <- input[i, ]
    }
  }
  input
}
