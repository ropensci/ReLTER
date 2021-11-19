message("\n---- Test get_network_research_topics() ----")

library(testthat)

skip_on_cran()

test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_network_research_topics(networkDEIMSID = "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3")
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of network research topics function constructs ‘tibble’ as expected", {
  result <- ReLTER::get_network_research_topics(networkDEIMSID = "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3")
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 2)
  expect_true(all(names(result) == c(
    "researchTopicsLabel", "researchTopicsUri"
  )))
  expect_type(result$researchTopicsLabel, "character")
  expect_type(result$researchTopicsUri, "character")
})

test_that("Wrong input (but URL) constructs a NULL object", {
  result <- ReLTER::get_network_research_topics(networkDEIMSID = "https://deims.org/network/ljhnhbkihubib")
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  result <- ReLTER::get_network_research_topics(networkDEIMSID = "ljhnhbkihubib")
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
})
