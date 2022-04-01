message("\n---- Test produce_network_points_map() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::produce_network_points_map(
        networkDEIMSID =
          "https://deims.org/networks/e904354a-f3a0-40ce-a9b5-61741f66c824",
          countryCode = "DEU"
      )
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of network point function constructs ‘tibble’ as expected", {
  result <- ReLTER::produce_network_points_map(
    networkDEIMSID =
      "https://deims.org/networks/e904354a-f3a0-40ce-a9b5-61741f66c824",
    countryCode = "DEU"
  )
  expect_s3_class(result, "tmap")
})

test_that("Wrong networkDEIMSID (but URL) constructs a NULL object", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER::produce_network_points_map(
    networkDEIMSID = "https://deims.org/networks/ljhnhbkihubib",
    countryCode = "DEU"
  )
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

test_that("Wrong networkDEIMSID (not URL) constructs an empty tibble", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER::produce_network_points_map(
    networkDEIMSID = "ljhnhbkihubib",
    countryCode = "DEU"
  )
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

test_that("Wrong countryCode constructs a NULL object", {
  result <- ReLTER::produce_network_points_map(
    networkDEIMSID =
      "https://deims.org/networks/e904354a-f3a0-40ce-a9b5-61741f66c824",
    countryCode = "EEA"
  )
  expect_s3_class(result, "tmap")

  expect_true(ncol(result$tm_shape$shp) == 4)
  expect_true(all(names(result$tm_shape$shp) == c(
    "title", "uri", "changed", "coordinates"
  )))

  expect_type(result$tm_shape$shp$title, "character")
  expect_type(result$tm_shape$shp$uri, "character")
  expect_type(result$tm_shape$shp$changed, "character")
  expect_type(result$tm_shape$shp$coordinates, "list")
})

test_that("Wrong both networkDEIMSID (but URL) and countryCode constructs
          a NULL object", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER::produce_network_points_map(
    networkDEIMSID = "https://deims.org/networks/ljhnhbkihubib",
    countryCode = "EEA"
  )
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

test_that("Wrong both networkDEIMSID (not URL) and countryCode constructs
          a NULL object", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER::produce_network_points_map(
    networkDEIMSID = "ljhnhbkihubib",
    countryCode = "EEA"
  )
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

test_that("Output of site affiliation information function constructs ‘sf' with
          valid geometries", {
  result <- ReLTER::produce_network_points_map(
    networkDEIMSID =
      "https://deims.org/networks/e904354a-f3a0-40ce-a9b5-61741f66c824",
    countryCode = "DEU"
  )
  expect_s3_class(result, "tmap")
})
