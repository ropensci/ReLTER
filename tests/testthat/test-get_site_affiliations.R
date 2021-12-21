message("\n---- Test get_site_affiliations() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER:::get_site_affiliations(
        deimsid = TESTURLSite
      )
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = TRUE) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of site affiliation function constructs 'tibble' as
          expected", {
  result <- ReLTER:::get_site_affiliations(
    deimsid = TESTURLSite
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 10)
  expect_true(all(names(result) == c(
    "title", "uri", "geoCoord", "country",
    "geoElev.avg", "geoElev.min",
    "geoElev.max", "geoElev.unit",
    "affiliation.networks", "affiliation.projects"
  )))

  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$geoCoord, "character")
  expect_type(result$country, "list")
})

test_that("Wrong input (but URL) constructs a NULL object", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER:::get_site_affiliations(
    deimsid = "https://deims.org/ljhnhbkihubib"
  )
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = TRUE) # restore test mode
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER:::get_site_affiliations(deimsid = "ljhnhbkihubib")
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = TRUE) # restore test mode
})
