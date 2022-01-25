message("\n---- Test get_sos_procedurelist() ----")

library(testthat)

skip_on_ci()

test_that("Output of SOS procedure list function constructs 'list'
          as expected", {
            result <- get_sos_procedurelist(
              sosHost = "http://getit.lteritalia.it/observations/sos/kvp?"
            )
            expect_type(result, "list")
          })
