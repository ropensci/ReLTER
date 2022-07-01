message("\n---- Test get_ilter_research_topics() ----")

library(testthat)

skip_on_cran()

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_ilter_research_topics(sitesNum = 10)
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of ILTER research topics function constructs 'tibble' as
          expected", {
  result <- ReLTER::get_ilter_research_topics(sitesNum = 20)
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 0)
  expect_true(all(names(result) == c()))
})

test_that("Wrong input (not a double) constructs an empty tibble", {
  result <- ReLTER::get_ilter_research_topics(sitesNum = "aa")
  expect_type(result, "NULL")
})
