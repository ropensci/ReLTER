message("\n---- Test get_ilter_observedProperties() ----")

library(testthat)

skip_on_cran()

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_ilter_observedProperties(sitesNum = 10)
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of ILTER observed properties function constructs 'tibble' as
          expected", {
  result <- ReLTER::get_ilter_observedProperties(sitesNum = 10)
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 2)
  expect_true(all(names(result) == c(
    "parameterLabel", "parameterUri"
  )))
  expect_type(result$parameterLabel, "character")
  expect_type(result$parameterUri, "character")
})

test_that("Wrong input (not a double) constructs an empty tibble", {
  result <- ReLTER::get_ilter_observedProperties(sitesNum = "aa")
  expect_type(result, "NULL")
})

test_that("If sitesNum is 0", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  lterILTERSites <- as.list(
    jsonlite::fromJSON("https://deims.org/api/sites")
  )
  number_sites <- length(lterILTERSites$title)
  result <- ReLTER::get_ilter_observedProperties()
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 2)
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})
