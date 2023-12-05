message("\n---- Test get_ilter_observedProperties() ----")

skip_on_cran()

test_that("Expect error if internet connection is down", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_error(
    httptest2::without_internet(
      result <- ReLTER::get_ilter_observedProperties(sitesNum = 10)
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of ILTER observed properties function constructs 'tibble' as
          expected", {
            withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER::get_ilter_observedProperties(sitesNum = 10)
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 2)
  expect_true(all(names(result) == c(
    "observedPropertiesLabel", "observedPropertiesUri"
  )))
  expect_type(result$observedPropertiesLabel, "character")
  expect_type(result$observedPropertiesUri, "character")
})

test_that("Wrong input (not a double) constructs an empty tibble", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER::get_ilter_observedProperties(sitesNum = "aa")
  expect_type(result, "NULL")
})
