message("\n---- Test get_activity_info() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_activity_info(
        activityid =
          "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845",
        show_map = FALSE
      )
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of activities information function constructs 'sf' and
          'tibble' as expected", {
  result <- ReLTER::get_activity_info(
    activityid =
      "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845",
    show_map = FALSE
  )
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 13)
  expect_true(all(names(result) == c(
    "title", "abstract", "keywords", "uri", "type", "created",
    "changed", "relatedSite",
    "contacts.corresponding", "contacts.metadataProvider", "boundaries",
    "observationParameters", "relatedResources"
  )))
  expect_type(result$title, "character")
  expect_type(result$boundaries, "list")
})

test_that("Wrong input (but URL) constructs a NULL object", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  expect_error(
    object <- ReLTER::get_activity_info(
      activityid = "https://deims.org/activity/ljhnhbkihubib",
      show_map = FALSE
    ),
    regexp = "Page Not Found"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  expect_error(
    object = ReLTER::get_activity_info(
      activityid = "ljhnhbkihubib",
      show_map = FALSE
    ),
    regexp = "Page Not Found"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

test_that("Output of get activities information function constructs 'sf' with
          valid geometries", {
  result <- ReLTER::get_activity_info(
    activityid =
      "https://deims.org/activity/8786fc6d-5d70-495c-b901-42f480182845",
    show_map = FALSE
  )
  result_valid <- sf::st_is_valid(result)
  expect_true(any(result_valid))
})

test_that("The activity don't have geo information", {
  result <- ReLTER::get_activity_info(
    activityid =
      "https://deims.org/activity/22983172-c53c-4ae9-9623-66f92cb222e3",
    show_map = FALSE
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 13)
  expect_true(all(names(result) == c(
    "title", "abstract", "keywords", "uri", "type", "created",   
    "changed", "relatedSite", "contacts.corresponding",
    "contacts.metadataProvider",
    "boundaries", "observationParameters", "relatedResources"
  )))
  expect_type(result$title, "character")
  expect_equal(result$boundaries, NA)
})
