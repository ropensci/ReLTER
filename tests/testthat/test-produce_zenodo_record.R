message("\n---- Test get_zenodo_data() ----")

test_that("Expect error if internet connection is down", {
  expect_error(
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