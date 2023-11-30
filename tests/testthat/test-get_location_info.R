message("\n---- Test get_location_info() ----")

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_location_info(
        locationid =
          "https://deims.org/location/12b38f3f-7e72-425a-80c7-7cad35ce4c7b",
        show_map = FALSE
      )
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of location information function constructs 'sf' and
          'tibble' as expected", {
  result <- ReLTER::get_location_info(
    locationid =
      "https://deims.org/location/12b38f3f-7e72-425a-80c7-7cad35ce4c7b",
    show_map = FALSE
  )
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 17)
  expect_true(all(names(result) == c(
    "title", "abstract", "uri",
    "locationType.label", "locationType.uri",
    "type", "created",
    "changed", "relatedSite.type", "relatedSite.title",
    "relatedSite.uri", "relatedSite.changed",
    "elevation.min", "elevation.max",
    "elevation.unit", "images", "boundaries"
  )))
  expect_type(result$title, "character")
  expect_type(result$boundaries, "list")
})

test_that("Wrong input (but URL) constructs a NULL object", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  expect_error(
    object <- ReLTER::get_location_info(
      locationid = "https://deims.org/location/ljhnhbkihubib",
      show_map = FALSE
    ),
    regexp = "Page Not Found"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  expect_error(
    object = ReLTER::get_location_info(
      locationid = "ljhnhbkihubib",
      show_map = FALSE
    ),
    regexp = "Page Not Found"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

test_that("Output of get location information function constructs 'sf' with
          valid geometries", {
  result <- ReLTER::get_location_info(
    locationid =
      "https://deims.org/location/12b38f3f-7e72-425a-80c7-7cad35ce4c7b",
    show_map = FALSE
  )
  result_valid <- sf::st_is_valid(result)
  expect_true(any(result_valid))
})
