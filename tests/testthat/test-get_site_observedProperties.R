message("\n---- Test get_site_observedProperties() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER:::get_site_info(
        deimsid = TESTURLSite,
        category = "observedProperties"
      )
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of site observed properties function constructs ‘tibble’ as
          expected", {
  result <- ReLTER:::get_site_info(
    deimsid = TESTURLSite,
    category = "observedProperties"
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 9)
  expect_true(all(names(result) == c(
    "title", "uri", "geoCoord", "country",
    "geoElev.avg", "geoElev.min", "geoElev.max", "geoElev.unit",
    "observedProperties"
  )))

  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$geoCoord, "character")
  expect_type(result$country, "list")
})

test_that("Wrong input (but URL) constructs a tibble with empty data", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER:::get_site_info(deimsid = "https://deims.org/ljhnhbkihubib")
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER:::get_site_info(deimsid = "ljhnhbkihubib")
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})
