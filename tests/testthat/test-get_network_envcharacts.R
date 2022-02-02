message("\n---- Test get_network_envcharacts() ----")

library(testthat)

skip_on_cran()

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_network_envcharacts(
        networkDEIMSID = TESTURLNetwork
      )
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

skip_if_offline(host = "deims.org")
skip_if(skip_in_test_mode)

test_that("Output of network environmental characteristics function constructs
          'tibble' as expected", {
  result <- ReLTER::get_network_envcharacts(
    networkDEIMSID = TESTURLNetwork
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
  expect_type(result$geoCoord, "character")
  expect_type(result$country, "list")
})

test_that("Wrong input (but URL) constructs a NULL object", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER::get_network_envcharacts(
    networkDEIMSID = "https://deims.org/network/ljhnhbkihubib"
  )
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER::get_network_envcharacts(networkDEIMSID = "ljhnhbkihubib")
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})
