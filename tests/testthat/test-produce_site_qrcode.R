message("\n---- Test produce_site_qrcode() ----")

library(testthat)

skip_if_offline(host = "deims.org")

test_that("Output of site affiliation function constructs ‘qr_code’ as
          expected", {
  result <- ReLTER::produce_site_qrcode(
    deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
  )
  expect_s3_class(result, "qr_code")
})
