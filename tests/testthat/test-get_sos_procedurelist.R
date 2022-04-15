message("\n---- Test get_sos_procedurelist() ----")

library(testthat)

test_that("Output of SOS procedure list function constructs 'list'
          as expected", {
            result <- get_sos_procedurelist(
              sosHost = "http://getit.lteritalia.it/observations/sos/kvp?"
            )
            expect_type(result, "list")
          })
