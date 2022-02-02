message("\n---- Test get_network_related_resources() ----")

library(testthat)

skip_on_cran()

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_network_related_resources(
        networkDEIMSID = TESTURLNetwork
      )
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

skip_if_offline(host = "deims.org")
skip_if(skip_in_test_mode)

test_that("Output of network related resources function constructs 'tibble' as
          expected", {
  result <- ReLTER::get_network_related_resources(
    networkDEIMSID = TESTURLNetwork
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 3)
  expect_true(all(names(result) == c(
    "relatedResourcesTitle",
    "uri",
    "relatedResourcesChanged"
  )))
  expect_type(result$uri, "character")
  expect_type(result$relatedResourcesTitle, "character")
  expect_type(result$relatedResourcesChanged, "character")
})

test_that("Wrong input (but URL) constructs a NULL object", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER::get_network_related_resources(
    networkDEIMSID = "https://deims.org/network/ljhnhbkihubib"
  )
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER::get_network_related_resources(
    networkDEIMSID = "ljhnhbkihubib"
  )
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})
