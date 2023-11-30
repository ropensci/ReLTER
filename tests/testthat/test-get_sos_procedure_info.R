message("\n---- Test get_sos_procedure_info() ----")

skip_if_offline(host = "getit.lteritalia.it")

test_that("Output of function constructs 'character' as expected", {
  result <- ReLTER::get_sos_procedure_info(
    sosURL = "http://getit.lteritalia.it/observations/service",
    procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/1286194C-A5DF-11DF-8ED7-1602DFD72097"
  )
  expect_type(result, "character")
  expect_true(all(names(attributes(result)) == c("id", "description")))
})