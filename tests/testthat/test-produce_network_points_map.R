message("\n---- Test produce_network_points_map() ----")

# Skip tests if running in test mode (offline/CI)
skip_if(skip_in_test_mode)
skip_if_offline(host = "deims.org")

test_that("Function returns a ggplot object for valid network and country", {
  result <- produce_network_points_map(
    networkDEIMSID = TESTURLNetwork,
    countryCode = "ITA"
  )
  expect_s3_class(result, "ggplot")
})

test_that("Output of network point function constructs ‘tibble’ as expected", {
  result <- ReLTER::produce_network_points_map(
    networkDEIMSID =
      "https://deims.org/networks/e904354a-f3a0-40ce-a9b5-61741f66c824",
    countryCode = "DEU"
  )
  expect_s3_class(result, "ggplot")
})

test_that("Wrong networkDEIMSID (not URL) constructs an empty tibble", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER::produce_network_points_map(
    networkDEIMSID = "ljhnhbkihubib",
    countryCode = "DEU"
  )
  expect_type(result, "NULL")
})

test_that("Function handles invalid network URL gracefully", {
  expect_message(
    result <- produce_network_points_map(
      networkDEIMSID = "https://deims.org/networks/invalid123",
      countryCode = "ITA"
    ),
    "requested page could not be found"
  )
  expect_null(result)
})

test_that("Function handles invalid network ID (not URL) gracefully", {
  expect_message(
    result <- produce_network_points_map(
      networkDEIMSID = "invalid123",
      countryCode = "ITA"
    ),
    "requested page could not be found"
  )
  expect_null(result)
})

test_that("Function handles invalid country code gracefully", {
  expect_message(
    result <- produce_network_points_map(
      networkDEIMSID = TESTURLNetwork,
      countryCode = "EEE"
    ),
    "map of country cannot be created"
  )
  expect_null(result)
})

test_that("Function handles sites with missing/invalid geometry", {
  # Example with a known site/network with missing geometry
  result <- produce_network_points_map(
    networkDEIMSID = TESTURLNetwork,
    countryCode = "ITA"
  )
  # Should print a message about invalid geometry, still returns ggplot
  expect_s3_class(result, "ggplot")
})

test_that("Function returns error if internet is down", {
  expect_error(
    httptest2::without_internet({
      produce_network_points_map(
        networkDEIMSID = TESTURLNetwork,
        countryCode = "ITA"
      )
    }),
    "GET"
  )
})
