message("\n---- Test get_ilter_generalinfo() ----")

library(testthat)

skip_on_cran()

test_that("Expect error if internet connection is down", {
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_ilter_generalinfo()
    ),
    "GET"
  )
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of ILTER general info function constructs 'tibble' as
          expected", {
  result <- ReLTER::get_ilter_generalinfo(country_name = NA, site_name = NA)
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 10)
  expect_true(all(names(result) == c(
    "title", "uri", "geoCoord", "country",
    "geoElev.avg", "geoElev.min",
    "geoElev.max", "geoElev.unit",
    "affiliation.networks", "affiliation.projects"
  )))

  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$geoCoord, "list")
  expect_type(result$country, "list")
  expect_type(result$geoElev.avg, "double")
  expect_type(result$geoElev.min, "double")
  expect_type(result$geoElev.max, "double")
  expect_type(result$geoElev.unit, "character")
  expect_type(result$affiliation.networks, "list")
  expect_type(result$affiliation.projects, "list")
})

test_that("Wrong input (not a character) of 'country_name'
          constructs an empty tibble", {
  expect_warning(
    ReLTER::get_ilter_generalinfo(country_name = "123"),
    regexp = "that is not among the countries stored"
  )
})

test_that("Wrong input (not a character) of 'site_name', but correct input of
          'country_name'', constructs an empty tibble", {
  result <- ReLTER::get_ilter_generalinfo(
    country_name = "Austri",
    site_name = "123"
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 10)
  expect_true(all(names(result) == c(
    "title", "uri", "geoCoord", "country",
    "geoElev.avg", "geoElev.min",
    "geoElev.max", "geoElev.unit",
    "affiliation.networks", "affiliation.projects"
  )))

  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$geoCoord, "list")
  expect_type(result$country, "list")
  expect_type(result$geoElev.avg, "integer")
  expect_type(result$geoElev.min, "integer")
  expect_type(result$geoElev.max, "integer")
  expect_type(result$geoElev.unit, "character")
  expect_type(result$affiliation.networks, "list")
  expect_type(result$affiliation.projects, "list")
})

test_that("Wrong input (not a character) of 'country_name' and 'site_name'
          constructs an empty tibble", {
  # TODO: write a test for that!
})

test_that("Output of function constructs 'sf' with valid geometries", {
  result <- get_ilter_generalinfo(
    country_name = "Austri",
    site_name = " Eisen"
  )
  result_valid <- (!is.na(result) & sf::st_is_valid(result))
  expect_true(any(result_valid))
})
