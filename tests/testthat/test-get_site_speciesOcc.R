message("\n---- Test get_site_speciesOccurrences() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_site_speciesOccurrences(
        deimsid = TESTURLSite,
        list_DS = c("gbif", "inat"),
        show_map = TRUE,
        limit = 20
      )
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of the function is 'list' as expected", {
  result <- ReLTER::get_site_speciesOccurrences(
    deimsid = TESTURLSite,
    list_DS = c("gbif", "inat"),
    show_map = FALSE,
    limit = 100
  )
  expect_type(result, "list")
  expect_true(ncol(result$gbif) == 81)
  expect_true(ncol(result$inat) == 152)
  expect_true(nrow(result$gbif) == 100)
  expect_true(nrow(result$inat) == 100)
})

test_that("Wrong input (but URL) constructs a NULL object", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER::get_site_speciesOccurrences(
    deimsid = "https://deims.org/ljhnhbkihubib",
    list_DS = c("gbif", "inat"),
    show_map = FALSE,
    limit = 100
  )
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER::get_site_speciesOccurrences(
    deimsid = "ljhnhbkihubib",
    list_DS = c("gbif", "inat"),
    show_map = FALSE,
    limit = 100
  )
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})
