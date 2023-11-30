message("\n---- Test taxon_id_worms_refine() ----")

test_that("Output of taxon worms function constructs 'tibble' as expected
with choiceNumber NULL", {
  # input_table <- tibble::tibble(
  #   ID = c(1, 2, 3, 4, 5, 6, 7),
  #   species = c(
  #     "Asterionella formosa", "Chrysococcus sp.",
  #     "Cryptomonas rostrata", "Dinobryon divergens",
  #     "Mallomonas akrokomos", "Melosira varians",
  #     "Cryptomonas rostrata"
  #   )
  # )
  # result <- ReLTER::taxon_id_worms(
  #   input = input_table,
  #   taxaColumn = 2,
  #   verbose = TRUE,
  #   refine = FALSE
  # )
  # result_refine <- ReLTER:::taxon_id_worms_refine(
  #   input = result,
  #   taxaColumn = 1,
  #   interaction = FALSE,
  #   choiceNumber = NULL
  # )
  # expect_s3_class(result_refine, "tbl_df")
  # expect_true(ncol(result_refine) == ncol(input_table) + 17)
  # expect_true(all(names(result_refine) == c(
  #   names(input_table[1]), "originalNameUsage",
  #   "scientificName", "scientificNameAuthorship", "taxonID",
  #   "taxonomicStatus", "synonyms", "taxonRank", "kingdom", "phylum",
  #   "class", "order", "family", "genus", "scientificNameID",
  #   "nOfWormsResults",  "wormsRecords"
  # )))
})

test_that("Output of taxon worms function constructs 'tibble' as expected
and choiceNumber is a number", {
  # input_table <- tibble::tibble(
  #   ID = c(1, 2, 3, 4, 5, 6, 7),
  #   species = c(
  #     "Asterionella formosa", "Chrysococcus sp.",
  #     "Cryptomonas rostrata", "Dinobryon divergens",
  #     "Mallomonas akrokomos", "Melosira varians",
  #     "Cryptomonas rostrata"
  #   )
  # )
  # result <- ReLTER::taxon_id_worms(
  #   input = input_table,
  #   taxaColumn = 2,
  #   verbose = TRUE,
  #   refine = FALSE
  # )
  # result_refine <- ReLTER:::taxon_id_worms_refine(
  #   input = result,
  #   taxaColumn = 1,
  #   interaction = FALSE,
  #   choiceNumber = 1
  # )
  # expect_s3_class(result_refine, "tbl_df")
  # expect_true(ncol(result_refine) == ncol(input_table) + 17)
  # expect_true(all(names(result_refine) == c(
  #   names(input_table[1]), "originalNameUsage",
  #   "scientificName", "scientificNameAuthorship", "taxonID",
  #   "taxonomicStatus", "synonyms", "taxonRank", "kingdom", "phylum",
  #   "class", "order", "family", "genus", "scientificNameID",
  #   "nOfWormsResults",  "wormsRecords"
  # )))
  # expect_lte(max(result_refine$nOfWormsResults), 1)
})
