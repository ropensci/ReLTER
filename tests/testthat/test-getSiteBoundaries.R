message("\n---- Test getSiteBoundaries() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::getSiteBoundaries(deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe')
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of site boundaries function constructs ‘sf’ and ‘tibble’ as expected", {
  result <- ReLTER::getSiteBoundaries(deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe')
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 3)
  expect_true(all(names(result) == c(
    "title", "uri", "boundaries"
  )))
  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$boundaries, "list")
})

test_that("Wrong input (but URL) constructs a NULL object", {
  result <- ReLTER::getSiteBoundaries(deimsid = "https://deims.org/ljhnhbkihubib")
  expect_type(result, "NULL")
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  result <- ReLTER::getSiteBoundaries(deimsid = "ljhnhbkihubib")
  expect_type(result, "NULL")
})

