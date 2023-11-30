message("\n---- Test taxon_id_worms() ----")

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
  #expect_true(ncol(result) == ncol(input_table) + 17)
  expect_true(all(names(result) == c(
    names(input_table[1]), "originalNameUsage",
    "scientificName", "scientificNameAuthorship", "taxonID",
    "taxonomicStatus", "synonyms", "taxonRank", "kingdom", "phylum",
    "class", "order", "family", "genus", "scientificNameID",
    "nOfWormsResults",  "wormsRecords"
  )))
})
