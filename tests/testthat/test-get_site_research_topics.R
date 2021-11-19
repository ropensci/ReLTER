message("\n---- Test get_site_research_topics() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_site_research_topics(
        deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of site reseearch topics function constructs ‘tibble’ as
          expected", {
  result <- ReLTER::get_site_research_topics(
    deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 9)
  expect_true(all(names(result) == c(
    "title", "uri", "geoCoord", "country",
    "geoElev.avg", "geoElev.min", "geoElev.max", "geoElev.unit",
    "researchTopics"
  )))

  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$geoCoord, "character")
  expect_type(result$country, "list")
  expect_type(result$geoElev.avg, "double")
  expect_type(result$geoElev.min, "integer")
  expect_type(result$geoElev.max, "integer")
  expect_type(result$geoElev.unit, "character")
  expect_type(result$researchTopics, "list")
})

test_that("Wrong input (but URL) constructs a tibble with empty data", {
  result <- ReLTER::get_site_research_topics(
    deimsid = "https://deims.org/ljhnhbkihubib"
  )
  expect_type(result, "NULL")
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  result <- ReLTER::get_site_research_topics(deimsid = "ljhnhbkihubib")
  expect_type(result, "NULL")
})
