message("\n---- Test get_site_envcharacts() ----")

test_that("Expect error if internet connection is down", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_error(
    httptest::without_internet(
      result <- ReLTER:::get_site_envcharacts(
        deimsid = TESTURLSite
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of site environmental characteristics function constructs
          'tibble' as expected", {
  result <- ReLTER:::get_site_envcharacts(
    deimsid = TESTURLSite
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 26)
  expect_true(all(names(result) == c(
    "title", "uri",
    "geoCoord", "country", "geoElev.avg", "geoElev.min",
    "geoElev.max", "geoElev.unit", "airTemperature.yearlyAverage",
    "airTemperature.monthlyAverage", "airTemperature.unit", "airTemperature.referencePeriod",
    "precipitation.yearlyAverage",
    "precipitation.monthlyAverage", "precipitation.unit", "precipitation.referencePeriod",
    "biogeographicalRegion",
    "biome", "ecosystemType", "eunisHabitat", "landforms",
    "geoBonBiome", "geology", "hydrology", "soils",
    "vegetation"
  )))

  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$geoCoord, "character")
  expect_type(result$country, "character")
})

test_that("Wrong input (but URL) constructs a tibble with empty data", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER:::get_site_envcharacts(
    deimsid = "https://deims.org/ljhnhbkihubib"
  )
  expect_type(result, "NULL")
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER:::get_site_envcharacts(deimsid = "ljhnhbkihubib")
  expect_type(result, "NULL")
})
