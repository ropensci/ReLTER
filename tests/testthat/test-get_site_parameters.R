message("\n---- Test get_site_parameters() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_site_info(
        deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
        category = "Parameters"
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of site parameters function constructs ‘tibble’ as
          expected", {
  result <- ReLTER::get_site_info(
    deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
    category = "Parameters"
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 9)
  expect_true(all(names(result) == c(
    "title", "uri", "geoCoord", "country",
    "geoElev.avg", "geoElev.min", "geoElev.max", "geoElev.unit",
    "parameter"
  )))

  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$geoCoord, "character")
  expect_type(result$country, "list")
})

test_that("Wrong input (but URL) constructs a tibble with empty data", {
  result <- ReLTER::get_site_info(deimsid = "https://deims.org/ljhnhbkihubib")
  expect_type(result, "NULL")
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  result <- ReLTER::get_site_info(deimsid = "ljhnhbkihubib")
  expect_type(result, "NULL")
})
