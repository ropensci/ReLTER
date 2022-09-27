message("\n---- Test get_sos_foi() ----")

library(testthat)

skip_if_offline(host = "getit.lteritalia.it")

test_that("Output of function constructs 'sf' and 'tibble' as expected in
           show_map parameter is TRUE, and the geometry is point", {
             result <- ReLTER::get_sos_foi(
               sosURL = "http://getit.lteritalia.it/observations/service",
               show_map = TRUE
             )
             expect_s3_class(result, "sf")
             expect_true(ncol(result) == 7)
             expect_true(all(names(result) == c(
               "typeSf", "description", "name", "foiID", "sampledFeature",
               "srsName", "geometry"
             )))
             expect_s3_class(result$geometry, "sfc_POINT")
           })

test_that("Output of function constructs 'tibble' as expected in
           show_map parameter is FALSE", {
             result <- ReLTER::get_sos_foi(
               sosURL = "http://getit.lteritalia.it/observations/service",
               show_map = FALSE
             )
             expect_s3_class(result, "tbl_df")
             expect_true(ncol(result) == 8)
             expect_true(all(names(result) == c(
               "typeSf", "description", "name", "foiID", "sampledFeature",
               "srsName", "lon", "lat"
             )))
           })
