message("\n---- Test get_sos_obs() ----")

library(testthat)
library(SPARQL)

skip_if_offline(host = "getit.lteritalia.it")

test_that("Output of function constructs 'tibble' as expected", {
             result <- ReLTER::get_sos_obs(
               sosURL = "http://getit.lteritalia.it/observations/service",
               procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/SI001469-SPNAirTemp", #720 Slowinski National Park
               show_map = TRUE
             )
             expect_s3_class(result, "tbl_df")
           })
