message("\n---- Test produce_site_parameters_pie() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::produce_site_parameters_pie(
        deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of chart pie of parameters function constructs 'sf' and
          'tibble' as expected", {
  result <- ReLTER::produce_site_parameters_pie(
    deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
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
  result <- ReLTER::produce_site_parameters_pie(
    deimsid = "https://deims.org/ljhnhbkihubib"
  )
  expect_type(result, "NULL")
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  result <- ReLTER::produce_site_parameters_pie(
    deimsid = "ljhnhbkihubib"
  )
  expect_type(result, "NULL")
})
