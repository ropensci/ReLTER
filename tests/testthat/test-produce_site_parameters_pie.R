message("\n---- Test produce_site_parameters_pie() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::produce_site_parameters_pie(
        deimsid = TESTURLSite
      )
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of chart pie of parameters function constructs 'sf' and
          'tibble' as expected", {
  result <- ReLTER::produce_site_parameters_pie(
    deimsid = TESTURLSite
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 9)
  expect_true(all(names(result) == c(
    "parameterGroups", "n",
    "freq", "label",
    "end", "start",
    "middle", "hjust",
    "vjust"
  )))
  expect_type(result$parameterGroups, "character")
  expect_type(result$n, "integer")
  expect_type(result$freq, "double")
  expect_type(result$label, "character")
  expect_type(result$end, "double")
  expect_type(result$start, "double")
  expect_type(result$middle, "double")
  expect_type(result$hjust, "double")
  expect_type(result$vjust, "double")
})

test_that("Wrong input (but URL) constructs a NULL object", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER::produce_site_parameters_pie(
    deimsid = "https://deims.org/ljhnhbkihubib"
  )
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER::produce_site_parameters_pie(
    deimsid = "ljhnhbkihubib"
  )
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})
