message("\n---- Test get_ilter_envcharacts() ----")

library(testthat)

skip_on_cran()

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_ilter_envcharacts(sitesNum = 5)
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of ILTER environmental characteristics function constructs
          ‘tibble’ as expected", {
  result <- ReLTER::get_ilter_envcharacts(sitesNum = 5)
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 29)
  expect_true(all(names(result) == c(
    "title", "uri",
    "created", "changed",
    "geoCoord", "country",
    "geoElev.avg", "geoElev.min", "geoElev.max", "geoElev.unit",
    "lterSiteClassification",
    "airTemperature.yearlyAverage",
    "airTemperature.monthlyAverage",
    "airTemperature.unit",
    "airTemperature.referencePeriod",
    "precipitation.yearlyAverage",
    "precipitation.monthlyAverage",
    "precipitation.unit",
    "precipitation.referencePeriod",
    "biogeographicalRegion",
    "biome",
    "ecosystemType",
    "eunisHabitat",
    "landforms",
    "geoBonBiome",
    "geology",
    "hydrology",
    "soils",
    "vegetation"
  )))

  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$geoCoord, "character")
  expect_type(result$country, "character")
})

test_that("Wrong input (not a double) constructs an empty tibble", {
  result <- ReLTER::get_ilter_envcharacts(sitesNum = "aa")
  expect_type(result, "NULL")
})
