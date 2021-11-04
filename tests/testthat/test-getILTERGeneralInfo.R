message("\n---- Test getILTERGeneralInfo() ----")

library(testthat)

skip_on_cran()

test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::getILTERGeneralInfo()
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of ILTER general info function constructs ‘tibble’ as expected", {
  result <- ReLTER::getILTERGeneralInfo(country_name = NA, site_name = NA)
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 10)
  expect_true(all(names(result) == c(
    "title", "uri", "geoCoord", "country", "geoElev.avg", "geoElev.min", 
    "geoElev.max", "geoElev.unit", "affiliation.networks", "affiliation.projects"
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
  # expect_true(sf::st_distance(
  #   sf::st_as_sfc(result$geoCoord, crs = 4326), 
  #   sf::st_sfc(sf::st_point(c(8.63403, 45.9547)), crs = 4326)
  # )[1,1] < 1e-4*units::as_units("m"))
})

test_that("Wrong input (not a character) of 'country_name' constructs an empty tibble", {
  expect_warning(
    ReLTER::getILTERGeneralInfo(country_name = '123'),
    regexp = "You have provided a country name"
  )
})

test_that("Wrong input (not a character) of 'site_name', but correct input of 'country_name', constructs an empty tibble", {
  result <- ReLTER::getILTERGeneralInfo(country_name = "Austri", site_name = '123')
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 10)
  expect_true(all(names(result) == c(
    "title", "uri", "geoCoord", "country", "geoElev.avg", "geoElev.min", 
    "geoElev.max", "geoElev.unit", "affiliation.networks", "affiliation.projects"
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

test_that("Wrong input (not a character) of 'country_name' and 'site_name' constructs an empty tibble", {
  # TODO: write a test for that!
})

test_that("Output of function constructs ‘sf' with valid geometries", {
  result <- getILTERGeneralInfo(country_name = "Austri", site_name =" Eisen")
  result_sp <- sf::as_Spatial(result$geoCoord)
  result_valid <- rgeos::gIsValid(result_sp, byid = FALSE, reason = TRUE)
  expect_type(result_valid, "character")
  expect_match(result_valid, "Valid Geometry")
})