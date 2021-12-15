message("\n---- Test get_activity_info() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_activity_info(
        activityid =
          "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845"
      )
    ),
    "GET"
  )
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
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
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  expect_error(
    object = ReLTER::get_activity_info(
      activityid = "https://deims.org/activity/ljhnhbkihubib"
    ),
    regexp = "Page Not Found"
  )
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  expect_error(
    object = ReLTER::get_activity_info(activityid = "ljhnhbkihubib"),
    regexp = "Page Not Found"
  )
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
})

test_that("Output of get activities information function constructs 'sf' with
          valid geometries", {
  result <- ReLTER::get_activity_info(
    activityid =
      "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845"
  )
  result_valid <- sf::st_is_valid(result)
  expect_true(any(result_valid))
})
