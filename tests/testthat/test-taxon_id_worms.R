message("\n---- Test taxon_id_worms() ----")

library(testthat)

test_that("Output of taxon worms function constructs 'tibble' as expected", {
  input_table <- tibble::tibble(
     ID = c(1, 2, 3, 4, 5, 6, 7),
     species = c(
     "Asterionella formosa", "Chrysococcus sp.",
     "Cryptomonas rostrata", "Dinobryon divergens",
     "Mallomonas akrokomos", "Melosira varians",
     "Cryptomonas rostrata"
   )
  )
  result <- ReLTER::taxon_id_worms(
    input = input_table,
    taxaColumn = 2,
    verbose = TRUE,
    refine = FALSE
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == length(input_table) + 10)
  expect_true(all(names(result) == c(
    names(input_table), "valid_name",
    "valid_authority", "valid_AphiaID",
    "status", "synonyms", "LSID", "url",
    "matchType", "nOfWormsResults", "wormsRecords"
  )))
})
