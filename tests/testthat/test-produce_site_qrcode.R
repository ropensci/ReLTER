message("\n---- Test produce_site_qrcode() ----")

library(testthat)

skip_if_offline(host = "deims.org")

test_that("Output of site affiliation function constructs ‘qr_code’ as
          expected", {
  result <- ReLTER::produce_site_qrcode(
    deimsid = TESTURLSite
  )
  expect_s3_class(result, "qr_code")
})

test_that("do_plot is TRUE", {
  result <- ReLTER::produce_site_qrcode(
    deimsid = TESTURLSite,
    do_plot = TRUE
  )
  expect_s3_class(result, "qr_code")
})