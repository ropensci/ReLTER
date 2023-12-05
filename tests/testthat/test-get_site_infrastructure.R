message("\n---- Test get_site_infrastructure() ----")

test_that("Expect error if internet connection is down", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_error(
    httptest2::without_internet(
      result <- ReLTER:::get_site_infrastructure(
        deimsid = TESTURLSite
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of site infrastructure function constructs ‘tibble’ as
          expected", {
  result <- ReLTER:::get_site_infrastructure(
    deimsid = TESTURLSite
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 21)
  expect_true(all(names(result) == c(
    "title", "uri", "geoCoord", "country",
    "geoElev.avg", "geoElev.min", "geoElev.max", "geoElev.unit",
    "accessibleAllYear", "accessType",
    "allPartsAccessible", "maintenanceInterval",
    "permanentPowerSupply", "operation.permanent",
    "operation.notes", "operation.siteVisitInterval",
    "notes", "collection",
    "data.policy.url", "data.policy.rights",
    "data.policy.notes"
  )))

  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$geoCoord, "character")
  expect_type(result$country, "character")
})

test_that("Wrong input (but URL) constructs a tibble with empty data", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER:::get_site_infrastructure(
    deimsid = "https://deims.org/ljhnhbkihubib"
  )
  expect_type(result, "NULL")
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER:::get_site_infrastructure(deimsid = "ljhnhbkihubib")
  expect_type(result, "NULL")
})
