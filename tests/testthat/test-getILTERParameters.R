message("\n---- Test getILTERParameters() ----")

library(testthat)

skip_on_cran()

test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::getILTERParameters(sitesNum = 10)
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of ILTER parameters function constructs ‘tibble’ as expected", {
  result <- ReLTER::getILTERParameters(sitesNum = 50)
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 2)
  expect_true(all(names(result) == c(
    "parameterLabel", "parameterUri"
  )))
  expect_type(result$parameterLabel, "character")
  expect_type(result$parameterUri, "character")
})

test_that("Wrong input (not a double) constructs an empty tibble", {
  result <- ReLTER::getILTERParameters(sitesNum = 'aa')
  expect_type(result, "NULL")
})
