message("\n---- Test createQRCode() ----")

library(testthat)

skip_if_offline(host = "deims.org")

# TODO quali test si potrebbero fare per questa funzione?

test_that("Output of site affiliation function constructs ‘xxx’ as expected", {
  ReLTER::createQRCode(deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe')
})

test_that("Wrong input (but URL) constructs a NULL object", {
  ReLTER::createQRCode(deimsid = 'https://deims.org/ljhnhbkihubib')
  expect_type(result, "NULL")
})
