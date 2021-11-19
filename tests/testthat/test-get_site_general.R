message("\n---- Test get_site_general() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_site_general(
        deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of site general info function constructs 'tibble' as
          expected", {
  result <- ReLTER::get_site_general(
    deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 22)
  expect_true(all(names(result) == c(
    "title", "uri", "geoCoord", "country",
    "geoElev.avg", "geoElev.min", "geoElev.max", "geoElev.unit",
    "generalInfo.abstract", "generalInfo.keywords", "generalInfo.status.label",
    "generalInfo.status.uri", "generalInfo.yearEstablished",
    "generalInfo.yearClosed", "generalInfo.hierarchy.parent",
    "generalInfo.hierarchy.children", "generalInfo.siteName",
    "generalInfo.shortName", "generalInfo.siteType",
    "generalInfo.protectionLevel", "generalInfo.landUse", "generalInfo.images"
  )))

  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$geoCoord, "character")
  expect_type(result$country, "list")
  expect_type(result$geoElev.avg, "double")
  expect_type(result$geoElev.min, "integer")
  expect_type(result$geoElev.max, "integer")
  expect_type(result$geoElev.unit, "character")

  expect_type(result$generalInfo.abstract, "character")
  expect_type(result$generalInfo.keywords, "list")
  expect_type(result$generalInfo.status.label, "character")
  expect_type(result$generalInfo.status.uri, "character")
  expect_type(result$generalInfo.yearEstablished, "integer")
  expect_type(result$generalInfo.yearClosed, "logical")
  expect_type(result$generalInfo.hierarchy.parent, "list")
  expect_type(result$generalInfo.hierarchy.children, "logical")
  expect_type(result$generalInfo.siteName, "character")
  expect_type(result$generalInfo.shortName, "character")
  expect_type(result$generalInfo.siteType, "character")
  expect_type(result$generalInfo.protectionLevel, "logical")
  expect_type(result$generalInfo.landUse, "logical")
  expect_type(result$generalInfo.images, "logical")
})

test_that("Wrong input (but URL) constructs a tibble with empty data", {
  result <- ReLTER::get_site_general(
    deimsid = "https://deims.org/ljhnhbkihubib"
  )
  expect_type(result, "NULL")
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  result <- ReLTER::get_site_general(
    deimsid = "ljhnhbkihubib"
  )
  expect_type(result, "NULL")
})
