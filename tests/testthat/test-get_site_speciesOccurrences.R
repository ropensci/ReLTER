message("\n---- Test get_site_speciesOccurrences() ----")

test_that("Expect error if internet connection is down", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_error(
    httptest::without_internet(
      result <- ReLTER::get_site_speciesOccurrences(
        deimsid = TESTURLSite,
        list_DS = "inat",
        show_map = FALSE,
        limit = 10
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of function constructs 'sf' and list as expected", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  list_DS <- c("inat", "gbif")
  limit <- 10
  result <- ReLTER::get_site_speciesOccurrences(
    deimsid = TESTURLSite,
    list_DS = list_DS,
    show_map = FALSE,
    limit = limit
  )
  expect_type(result, "list")
  expect_identical(sort(names(result)), sort(list_DS))
  expect_true(length(result) == length(list_DS))
  for (i in list_DS) {
    expect_s3_class(result[[i]], "sf")
  }
})

test_that("Wrong input (but URL) constructs a tibble with empty data", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER::get_site_speciesOccurrences(
    deimsid = "https://deims.org/ljhnhbkihubib",
    list_DS = list_DS,
    show_map = FALSE,
    limit = limit
  )
  expect_type(result, "NULL")
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER::get_site_speciesOccurrences(
    deimsid = "ljhnhbkihubib",
    list_DS = list_DS,
    show_map = FALSE,
    limit = limit
  )
  expect_type(result, "NULL")
})
