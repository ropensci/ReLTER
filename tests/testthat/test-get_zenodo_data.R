message("\n---- Test get_zenodo_data() ----")

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

skip_if_offline(host = "zenodo.org")

test_that("The input is a Zenodo DOI but is not a dataset type,
          constructs a NULL object", {
  result <- ReLTER::get_zenodo_data(
    doi = "10.5281/zenodo.6469530", # test dataset
    rdata_exist = FALSE
  )
  expect_type(result, "NULL")
})

test_that("The input is not a Zenodo DOI constructs a NULL object", {
  result <- ReLTER::get_zenodo_data(
    doi = "10.3897/rio.8.e82597", # test dataset
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
