message("\n---- Test get_activity_info() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_activity_info(
        activityid =
          "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845"
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of activities information function constructs 'sf' and
          'tibble' as expected", {
  result <- ReLTER::get_activity_info(
    activityid =
      "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845"
  )
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
  expect_error(
    object = ReLTER::get_activity_info(
      activityid = "https://deims.org/activity/ljhnhbkihubib"
    ),
    regexp = "Page Not Found"
  )
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  expect_error(
    object = ReLTER::get_activity_info(activityid = "ljhnhbkihubib"),
    regexp = "Page Not Found"
  )
})

test_that("Output of get activities information function constructs 'sf' with
          valid geometries", {
  result <- ReLTER::get_activity_info(
    activityid =
      "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845"
  )
  result_sp <- sf::as_Spatial(result$boundaries)
  result_valid <- rgeos::gIsValid(result_sp, byid = FALSE, reason = TRUE)
  expect_type(result_valid, "character")
  expect_match(result_valid, "Valid Geometry")
})
