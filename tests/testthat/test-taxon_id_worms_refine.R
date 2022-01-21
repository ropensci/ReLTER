message("\n---- Test taxon_id_worms_refine() ----")

library(testthat)

test_that("Output of taxon worms function constructs 'tibble' as expected
with choiceNumber NULL", {
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
  result_refine <- ReLTER:::taxon_id_worms_refine(
    input = result,
    taxaColumn = 1,
    interaction = FALSE,
    choiceNumber = NULL
  )
  expect_s3_class(result_refine, "tbl_df")
  expect_true(ncol(result_refine) == length(input_table) + 10)
  expect_true(all(names(result_refine) == c(
    names(input_table), "valid_name",
    "valid_authority", "valid_AphiaID",
    "status", "synonyms", "LSID", "url",
    "matchType", "nOfWormsResults", "wormsRecords"
  )))
})

test_that("Output of taxon worms function constructs 'tibble' as expected
and choiceNumber is a number", {
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
  result_refine <- ReLTER:::taxon_id_worms_refine(
    input = result,
    taxaColumn = 1,
    interaction = FALSE,
    choiceNumber = 1
  )
  expect_s3_class(result_refine, "tbl_df")
  expect_true(ncol(result_refine) == length(input_table) + 10)
  expect_true(all(names(result_refine) == c(
    names(input_table), "valid_name",
    "valid_authority", "valid_AphiaID",
    "status", "synonyms", "LSID", "url",
    "matchType", "nOfWormsResults", "wormsRecords"
  )))
  expect_lte(max(result_refine$nOfWormsResults), 1)
})
