message("\n---- Test get_network_research_topics() ----")

skip_on_cran()

test_that("Expect error if internet connection is down", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_network_research_topics(
        networkDEIMSID = TESTURLNetwork
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")
skip_if(skip_in_test_mode)

test_that("Output of network research topics function constructs ‘tibble’ as
          expected", {
  result <- ReLTER::get_network_research_topics(
    networkDEIMSID = TESTURLNetwork
  )
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 0)
  expect_true(all(names(result) == c()))
})

test_that("Wrong input (but URL) constructs a NULL object", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER::get_network_research_topics(
    networkDEIMSID =
      "https://deims.org/network/ljhnhbkihubib")
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER::get_network_research_topics(
    networkDEIMSID = "ljhnhbkihubib")
  expect_true(is.null(result))
  expect_true(is.null(ncol(result)))
  expect_true(length(result) == 0)
})
