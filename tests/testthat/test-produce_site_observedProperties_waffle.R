message("\n---- Test produce_site_observedProperties_waffle() ----")

test_that("Expect error if internet connection is down", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_error(
    httptest2::without_internet(
      ReLTER::produce_site_observedProperties_waffle(
        deimsid = TESTURLSite
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output tibble has expected structure", {
  result <- suppressMessages(
    ReLTER::produce_site_observedProperties_waffle(
      deimsid = TESTURLSite
    )
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(ncol(result), 4)
  expect_equal(
    names(result),
    c("parameterGroups", "n", "freq", "label")
  )
  expect_type(result$parameterGroups, "character")
  expect_type(result$n, "integer")
  expect_type(result$freq, "double")
  expect_type(result$label, "character")
})

test_that("Output tibble has consistent values", {
  result <- suppressMessages(
    ReLTER::produce_site_observedProperties_waffle(
      deimsid = TESTURLSite
    )
  )
  # freq should sum to 1
  expect_equal(sum(result$freq), 1, tolerance = 1e-6)
  # n should be positive integers
  expect_true(all(result$n > 0L))
  # label should match freq
  expected_labels <- paste0(round(result$freq, 2) * 100, "%")
  expect_equal(result$label, expected_labels)
})

test_that("Wrong DEIMS ID (valid URL format) returns NULL", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- suppressMessages(
    ReLTER::produce_site_observedProperties_waffle(
      deimsid = "https://deims.org/ljhnhbkihubib"
    )
  )
  expect_null(result)
})

test_that("Wrong DEIMS ID (not a URL) returns NULL", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- suppressMessages(
    ReLTER::produce_site_observedProperties_waffle(
      deimsid = "ljhnhbkihubib"
    )
  )
  expect_null(result)
})
