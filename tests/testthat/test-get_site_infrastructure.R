message("\n---- Test get_site_infrastructure() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_site_infrastructure(
        deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
      )
    ),
    "GET"
  )
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of site infrastructure function constructs ‘tibble’ as
          expected", {
  result <- ReLTER::get_site_infrastructure(
    deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 21)
  expect_true(all(names(result) == c(
    "title", "uri", "geoCoord", "country",
    "geoElev.avg", "geoElev.min", "geoElev.max", "geoElev.unit",
    "generalInfo.accessibleAllYear", "generalInfo.accessType",
    "generalInfo.allPartsAccessible", "generalInfo.maintenanceInterval",
    "generalInfo.permanentPowerSupply", "generalInfo.operation.permanent",
    "generalInfo.operation.notes", "generalInfo.operation.siteVisitInterval",
    "generalInfo.notes", "generalInfo.collection",
    "generalInfo.data.policy.url", "generalInfo.data.policy.rights",
    "generalInfo.data.policy.notes"
  )))

  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$geoCoord, "character")
  expect_type(result$country, "list")
})

test_that("Wrong input (but URL) constructs a tibble with empty data", {
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  result <- ReLTER::get_site_infrastructure(
    deimsid = "https://deims.org/ljhnhbkihubib"
  )
  expect_type(result, "NULL")
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  result <- ReLTER::get_site_infrastructure(deimsid = "ljhnhbkihubib")
  expect_type(result, "NULL")
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
})
