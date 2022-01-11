message("\n---- Test produce_site_parameters_waffle() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::produce_site_parameters_waffle(
        deimsid = TESTURLSite
      )
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of chart waffle of parameters function constructs 'sf' and
          'tibble’ as expected", {
  result <- ReLTER::produce_site_parameters_waffle(
    deimsid = TESTURLSite
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 4)
  expect_true(all(names(result) == c(
    "parameterGroups", "n", "freq", "label"
  )))
  expect_type(result$parameterGroups, "character")
  expect_type(result$n, "integer")
  expect_type(result$freq, "double")
  expect_type(result$label, "character")
})

test_that("Wrong input (but URL) constructs a NULL object", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER::produce_site_parameters_waffle(
    deimsid = "https://deims.org/ljhnhbkihubib"
  )
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER::produce_site_parameters_waffle(
    deimsid = "ljhnhbkihubib"
  )
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})
