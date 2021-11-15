message("\n---- Test get_network_sites() ----")

library(testthat)

skip_on_cran()

# TODO: non capisco perché questo test dia errore
test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_network_sites(networkDEIMSID = "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3")
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of network sites information function constructs ‘sf' and 'data.frame’ as expected", {
  result <- ReLTER::get_network_sites(networkDEIMSID = "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3")
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 4)
  expect_true(all(names(result) == c(
    "title", "changed", "uri", "coordinates"
  )))
  expect_type(result$title, "character")
  expect_type(result$changed, "character")
  expect_type(result$uri, "character")
  expect_type(result$coordinates, "list")
})

test_that("Wrong input (but URL) constructs a NULL object", {
  result <- ReLTER::get_network_sites(networkDEIMSID = "https://deims.org/network/ljhnhbkihubib")
  expect_type(result, "NULL")
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  result <- ReLTER::get_network_sites(activityid = "ljhnhbkihubib")
  expect_type(result, "NULL")
})

test_that("Output of get activities information function constructs ‘sf' with valid geometries", {
  result <- ReLTER::get_network_sites(
    networkDEIMSID = "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
  )
  result_sp <- sf::as_Spatial(result$coordinates)
  result_valid <- rgeos::gIsValid(result_sp, byid = FALSE, reason = TRUE)
  expect_type(result_valid, "character")
  expect_match(result_valid, "Valid Geometry")
})
