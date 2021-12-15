message("\n---- Test get_network_envcharacts() ----")

library(testthat)

skip_on_cran()

test_that("Expect error if internet connection is down", {
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_network_envcharacts(
        networkDEIMSID = TESTURLNetwork
      )
    ),
    "GET"
  )
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
})

skip_if_offline(host = "deims.org")
skip_on_ci()

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
  #expect_type(result$geoElev.avg, "double")
  #expect_type(result$geoElev.min, "double")
  #expect_type(result$geoElev.max,"double") 
  # in alcuni casi potrebbe essere "double"
  expect_type(result$geoElev.unit, "character")

  expect_type(result$envCharacteristics.airTemperature.avg, "double")
  expect_type(result$envCharacteristics.airTemperature.min, "double")
  expect_type(result$envCharacteristics.airTemperature.max, "double")
  expect_type(result$envCharacteristics.airTemperature.unit, "character")
  #expect_type(result$envCharacteristics.precipitation.annual, "double")
  #expect_type(result$envCharacteristics.precipitation.min, "double")
  #expect_type(result$envCharacteristics.precipitation.max, "double")
  expect_type(result$envCharacteristics.precipitation.unit, "character")
  expect_type(result$envCharacteristics.biogeographicalRegion, "character")
  expect_type(result$envCharacteristics.biome, "character")
  expect_type(result$envCharacteristics.ecosystemType, "list")
  expect_type(result$envCharacteristics.eunisHabitat, "list")
  #expect_type(result$envCharacteristics.landforms, "character")
  #expect_type(result$envCharacteristics.geoBonBiome, "character")
  #expect_type(result$envCharacteristics.geology, "character")
  #expect_type(result$envCharacteristics.hydrology, "character")
  #expect_type(result$envCharacteristics.soils, "character")
  #expect_type(result$envCharacteristics.vegetation, "character")
})

test_that("Wrong input (but URL) constructs a NULL object", {
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  result <- ReLTER::get_network_envcharacts(
    networkDEIMSID = "https://deims.org/network/ljhnhbkihubib"
  )
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  result <- ReLTER::get_network_envcharacts(networkDEIMSID = "ljhnhbkihubib")
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
})
