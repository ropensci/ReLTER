message("\n---- Test get_activity_info() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_zenodo_data(
        doi = "10.5281/zenodo.7041152", # test dataset
        rdata_exist = FALSE
      )
    ),
    "GET"
  )
})

test_that("The input is not a Zenodo DOI (but DOI) constructs a NULL object", {
  result <- ReLTER::get_zenodo_data(
    doi = "10.1109/5.771073", # test dataset
    rdata_exist = FALSE
  )
  expect_type(result, "NULL")
})

test_that("Wrong input (not DOI) constructs an empty tibble", {
  result <- ReLTER::get_zenodo_data(
    doi = "dakoobdadas", # test dataset
    rdata_exist = FALSE
  )
  expect_type(result, "NULL")
})

test_that("Wrong input .RData or .rds file exist in the record ", {
  result <- ReLTER::get_zenodo_data(
    doi = "10.5281/zenodo.5575831", # test dataset
    rdata_exist = TRUE
  )
  expect_type(result, "NULL")
})