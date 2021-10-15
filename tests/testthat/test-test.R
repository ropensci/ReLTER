#  test the package ----
# To set up your package to use testthat, run:
# usethis::use_testthat()

# This will:
# 1. Create a tests/testthat directory.
# 2. Add testthat to the Suggests field in the DESCRIPTION.
# 3. Create a file tests/testthat.R that runs all your tests when R CMD check runs. (You’ll learn more about that in automated checking.)
# Once you’re set up the workflow is simple:
# 1. Modify your code or tests.
# 2. Test your package with Ctrl/Cmd + Shift + T or 
# devtools::test()
# 3. Repeat until all tests pass

#  check the package ----
devtools::check()

#  test the functions ----
library(testthat)


test_that("Site affiliation function constructs ‘tibble’ as expected", {
  result <- ReLTER::getSiteAffiliations(deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe')
  
  # It is a tibble
  expect_true(tibble::is_tibble(result))
  
  # It have rigth number of columns
  n_col <- length(result)
  expect_equal(n_col, 10)
  
  # It have rigth type of columns content
  result_type <- as_tibble(sapply(result, class))
  result_expectation_type <- tibble(
    value = c("character", "character", "character", "list", "numeric", "integer", "integer", "character", "list", "list")
  )
  expect_equal(result_type, result_expectation_type)
  
  # It have rigth columns name
  result_name <- as_tibble(names(result))
    result_expectation_name <- tibble(
      value = c("title", "uri", "geoCoord", "country", "geoElev.avg", "geoElev.min", "geoElev.max", "geoElev.unit", "affiliation.networks", "affiliation.projects")
    )
  expect_equal(result_name, result_expectation_name)
})



