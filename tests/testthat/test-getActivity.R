message("\n---- Test getActivity() ----")

library(testthat)
library(rgeos)
library(sf)

test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::getActivity(activityid = "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845")
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of activities information function constructs ‘sf' and 'tibble’ as expected", {
  result <- ReLTER::getActivity(activityid = "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845")
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 2)
  expect_true(all(names(result) == c(
    "title", "boundaries"
  )))
  expect_type(result$title, "character")
  expect_type(result$boundaries, "list")
})

test_that("Wrong input (but URL) constructs a NULL object", {
  result <- ReLTER::getActivity(activityid = "https://deims.org/activity/ljhnhbkihubib")
  expect_type(result, "NULL")
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  result <- ReLTER::getActivity(activityid = "ljhnhbkihubib")
  expect_type(result, "NULL")
})

test_that("Output of get activities information function constructs ‘sf' with valid geometries", {
  result <- ReLTER::getActivity(activityid = "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845")
  result_sp <- sf::as_Spatial(result$boundaries)
  result_valid <- rgeos::gIsValid(result_sp, byid = FALSE, reason = TRUE)
  expect_type(result_valid, "character")
  expect_match(result_valid, "Valid Geometry")
})
