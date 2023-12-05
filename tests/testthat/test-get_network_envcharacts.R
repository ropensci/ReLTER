message("\n---- Test get_network_envcharacts() ----")

skip_on_cran()

test_that("Expect error if internet connection is down", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_error(
    httptest2::without_internet(
      result <- ReLTER::get_network_envcharacts(
        networkDEIMSID = TESTURLNetwork
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

skip_if(skip_in_test_mode)

test_that("Output of network environmental characteristics function constructs
          'tibble' as expected", {
  result <- ReLTER::get_network_envcharacts(
    networkDEIMSID = TESTURLNetwork
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 33)
  expect_true(all(names(result) == c(
    "title", "uri", "created", "changed",
    "geoCoord", "country", "geoElev.avg", "geoElev.min",
    "geoElev.max", "geoElev.unit", "lterSiteClassification", "airTemperature.yearlyAverage",
    "airTemperature.monthlyAverage", "airTemperature.unit", "airTemperature.referencePeriod",
    "precipitation.yearlyAverage",
    "precipitation.monthlyAverage", "precipitation.unit", "precipitation.referencePeriod",
    "biogeographicalRegion",
    "biome", "ecosystemType", "eunisHabitat", "landforms",
    "geoBonBiome", "geology", "hydrology", "soils",
    "vegetation", "airTemperature.referencePeriod.label",
    "airTemperature.referencePeriod.uri", "precipitation.referencePeriod.label",
    "precipitation.referencePeriod.uri"
  )))

  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$geoCoord, "character")
  expect_type(result$country, "character")
})

test_that("Wrong input (but URL) constructs a NULL object", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER::get_network_envcharacts(
    networkDEIMSID = "https://deims.org/network/ljhnhbkihubib"
  )
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER::get_network_envcharacts(networkDEIMSID = "ljhnhbkihubib")
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
})
