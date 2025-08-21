message("\n---- Test produce_site_map() ----")

# Skip tests if running in test mode (offline/CI)
skip_if(skip_in_test_mode)

VALID_SITE <- "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
INVALID_SITE_URL <- "https://deims.org/invalid123"
INVALID_SITE_ID <- "invalid123"

test_that("Function returns a combined ggplot object for valid site", {
  result <- suppressWarnings(produce_site_map(
    deimsid = VALID_SITE
  ))
  expect_s3_class(result, "ggplot")
})

test_that("Function allows custom scale, arrow, and inset positions", {
  result <- suppressWarnings(produce_site_map(
    deimsid = VALID_SITE,
    scale_location = "tr",
    arrow_location = "bl",
    inset_position = "tl"
  ))
  expect_s3_class(result, "ggplot")
})

test_that("Function handles no internet connection gracefully", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_error(
    httptest2::without_internet(
      produce_site_map(VALID_SITE)
    ),
    "GET|could not resolve host|Error"
  )
})
