message("\n---- Test get_network_parameters() ----")

library(testthat)

skip_on_cran()

test_that("Expect error if internet connection is down", {
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_network_parameters(
        networkDEIMSID =
          "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
      )
    ),
    "GET"
  )
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of network parameters function constructs 'tibble' as
          expected", {
  result <- ReLTER::get_network_parameters(
    networkDEIMSID =
      "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 2)
  expect_true(all(names(result) == c(
    "parameterLabel", "parameterUri"
  )))
  expect_type(result$parameterLabel, "character")
  expect_type(result$parameterUri, "character")
})

test_that("Wrong input (but URL) constructs a NULL object", {
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  result <- ReLTER::get_network_parameters(
    networkDEIMSID = "https://deims.org/network/ljhnhbkihubib"
  )
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  result <- ReLTER::get_network_parameters(networkDEIMSID = "ljhnhbkihubib")
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
})   

