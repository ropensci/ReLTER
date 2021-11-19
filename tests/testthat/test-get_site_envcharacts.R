message("\n---- Test get_site_envcharacts() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_site_envcharacts(
        deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of site environmental characteristics function constructs
          'tibble' as expected", {
  result <- ReLTER::get_site_envcharacts(
    deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 26)
  expect_true(all(names(result) == c(
    "title", "uri", "geoCoord", "country",
    "geoElev.avg", "geoElev.min", "geoElev.max", "geoElev.unit",
    "envCharacteristics.airTemperature.avg",
    "envCharacteristics.airTemperature.min",
    "envCharacteristics.airTemperature.max",
    "envCharacteristics.airTemperature.unit",
    "envCharacteristics.precipitation.annual",
    "envCharacteristics.precipitation.min",
    "envCharacteristics.precipitation.max",
    "envCharacteristics.precipitation.unit",
    "envCharacteristics.biogeographicalRegion", "envCharacteristics.biome",
    "envCharacteristics.ecosystemType", "envCharacteristics.eunisHabitat",
    "envCharacteristics.landforms", "envCharacteristics.geoBonBiome",
    "envCharacteristics.geology", "envCharacteristics.hydrology",
    "envCharacteristics.soils", "envCharacteristics.vegetation"
  )))

  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$geoCoord, "list")
  expect_type(result$country, "list")
  expect_type(result$geoElev.avg, "double")
  expect_type(result$geoElev.min, "double")
  expect_type(result$geoElev.max, "double")
  expect_type(result$geoElev.unit, "character")

  expect_type(result$envCharacteristics.airTemperature.avg, "double")
  expect_type(result$envCharacteristics.airTemperature.min, "double")
  expect_type(result$envCharacteristics.airTemperature.max, "double")
  expect_type(result$envCharacteristics.airTemperature.unit, "character")
  expect_type(result$envCharacteristics.precipitation.annual, "integer")
  expect_type(result$envCharacteristics.precipitation.min, "logical")
  expect_type(result$envCharacteristics.precipitation.max, "integer")
  expect_type(result$envCharacteristics.precipitation.unit, "character")
  expect_type(result$envCharacteristics.biogeographicalRegion, "character")
  expect_type(result$envCharacteristics.biome, "character")
  expect_type(result$envCharacteristics.ecosystemType, "list")
  expect_type(result$envCharacteristics.eunisHabitat, "list")
  expect_type(result$envCharacteristics.landforms, "logical")
  expect_type(result$envCharacteristics.geoBonBiome, "character")
  expect_type(result$envCharacteristics.geology, "character")
  expect_type(result$envCharacteristics.hydrology, "character")
  expect_type(result$envCharacteristics.soils, "logical")
  expect_type(result$envCharacteristics.vegetation, "logical")
})

test_that("Wrong input (but URL) constructs a tibble with empty data", {
  result <- ReLTER::get_site_envcharacts(
    deimsid = "https://deims.org/ljhnhbkihubib"
  )
  expect_type(result, "NULL")
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  result <- ReLTER::get_site_envcharacts(deimsid = "ljhnhbkihubib")
  expect_type(result, "NULL")
})
