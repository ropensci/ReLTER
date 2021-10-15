message("\n---- Test mapNetworkPoint() ----")

library(testthat)

# TODO: Alcuni errori che non capisco a cosa siano dovuti, forse al warning per questa combinazione di Network e country code? 
test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::mapNetworkPoint(
        networkDEIMSID = 'https://deims.org/networks/e904354a-f3a0-40ce-a9b5-61741f66c824',
        countryCode = 'DEU'
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of network point function constructs ‘tibble’ as expected", {
  result <- ReLTER::mapNetworkPoint(
    networkDEIMSID = 'https://deims.org/networks/e904354a-f3a0-40ce-a9b5-61741f66c824',
    countryCode = 'DEU'
  )
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "sf")
  expect_true(ncol(result) == 4)
  expect_true(all(names(result) == c(
    "title", "uri", "changed", "coordinates"
  )))

  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$changed, "character")
  expect_type(result$coordinates, "list")
})

test_that("Wrong networkDEIMSID (but URL) constructs a NULL object", {
  result <- ReLTER::mapNetworkPoint(
    networkDEIMSID = 'https://deims.org/networks/ljhnhbkihubib',
    countryCode = 'DEU'
  )
  expect_type(result, "NULL")
})

test_that("Wrong networkDEIMSID (not URL) constructs an empty tibble", {
  result <- ReLTER::mapNetworkPoint(
    networkDEIMSID = 'ljhnhbkihubib',
    countryCode = 'DEU'
  )
  expect_type(result, "NULL")
})

test_that("Wrong countryCode constructs a NULL object", {
  result <- ReLTER::mapNetworkPoint(
    networkDEIMSID = 'https://deims.org/networks/e904354a-f3a0-40ce-a9b5-61741f66c824',
    countryCode = 'EEA'
  )
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "sf")
  expect_true(ncol(result) == 4)
  expect_true(all(names(result) == c(
    "title", "uri", "changed", "coordinates"
  )))
  
  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$changed, "character")
  expect_type(result$coordinates, "list")
})

test_that("Wrong both networkDEIMSID (but URL) and countryCode constructs a NULL object", {
  result <- ReLTER::mapNetworkPoint(
    networkDEIMSID = 'https://deims.org/networks/ljhnhbkihubib',
    countryCode = 'EEA'
  )
  expect_type(result, "NULL")
})

test_that("Wrong both networkDEIMSID (not URL) and countryCode constructs a NULL object", {
  result <- ReLTER::mapNetworkPoint(
    networkDEIMSID = 'ljhnhbkihubib',
    countryCode = 'EEA'
  )
  expect_type(result, "NULL")
})

test_that("Output of site affiliation information function constructs ‘sf' with valid geometries", {
  result <- ReLTER::mapNetworkPoint(
    networkDEIMSID = 'https://deims.org/networks/e904354a-f3a0-40ce-a9b5-61741f66c824',
    countryCode = 'DEU'
  )
  result_sp <- sf::as_Spatial(result$coordinates)
  result_valid <- gIsValid(result_sp, byid = FALSE, reason = TRUE)
  expect_type(result_valid, "character")
  expect_match(result_valid, "Valid Geometry")
})
