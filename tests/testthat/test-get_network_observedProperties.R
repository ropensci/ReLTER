message("\n---- Test get_network_observedProperties() ----")

skip_on_cran()

skip_if_offline(host = "deims.org")

test_that("Expect error if internet connection is down", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_error(
    httptest::without_internet(
      result <- ReLTER::get_network_observedProperties(
        networkDEIMSID = TESTURLNetwork
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

skip_if(skip_in_test_mode)

test_that("Output of network observed properties function constructs 'tibble' as
          expected", {
  result <- ReLTER::get_network_observedProperties(
    networkDEIMSID = TESTURLNetwork
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 2)
  expect_true(all(names(result) == c(
    "observedPropertiesLabel", "observedPropertiesUri"
  )))
  expect_type(result$observedPropertiesLabel, "character")
  expect_type(result$observedPropertiesUri, "character")
})

test_that("Wrong input (but URL) constructs a NULL object", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER::get_network_observedProperties(
    networkDEIMSID = "https://deims.org/network/ljhnhbkihubib"
  )
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER::get_network_observedProperties(
    networkDEIMSID = "ljhnhbkihubib"
  )
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
})
