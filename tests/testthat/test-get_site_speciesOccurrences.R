message("\n---- Test get_site_speciesOccurrences() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
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
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of activities information function constructs 'sf' and
          'tibble' as expected", {
            result <- ReLTER::get_site_speciesOccurrences(
              deimsid = TESTURLSite,
              list_DS = "inat",
              show_map = FALSE,
              limit = 10
            )
            expect_s3_class(result, "sf")
            expect_s3_class(result, "tbl_df")
            expect_true(ncol(result) == 13)
            expect_true(all(names(result) == c(
              "title", "abstract", "keywords", "uri", "type", "created",
              "changed", "relatedSite",
              "contacts.corresponding", "contacts.metadataProvider", "boundaries",
              "observationParameters", "relatedResources"
            )))
            expect_type(result$title, "character")
            expect_type(result$boundaries, "list")
          })