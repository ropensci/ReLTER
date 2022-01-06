message("\n---- Test get_site_MODIS() ----")

library(testthat)
skip_if_offline(host = "deims.org")
skip_if_offline(host = "https://e4ftl01.cr.usgs.gov/")

# Check for requested products or bands list (function returns NULL)
test_that("When show_products is TRUE, or show_bands is character,
          result is NULL", {
  result1 <- get_site_MODIS(show_products = TRUE)
  result2 <- get_site_MODIS(show_bands = "M*D13Q1")
  expect_type(result1, "NULL")
  expect_type(result2, "NULL")
})

# Check for password and username
test_that("Username and password are character strings", {
  result1 <- get_site_MODIS(
          deimsid = "https://deims.org/86e3a1ca-b2ba-4b06-b096-71447df52841",
          earthdata_user = "abc",
          earthdata_passwd = NULL,
          from_date = "2017.01.01",
          to_date = "2017.30.03"
          )
  result2 <- get_site_MODIS(
          deimsid = "https://deims.org/86e3a1ca-b2ba-4b06-b096-71447df52841",
          earthdata_user = NULL,
          earthdata_passwd = "abc",
          from_date = "2017.01.01",
          to_date = "2017.30.03"
          )
  expect_error(result1, regexp = "Missing")
  expect_error(result2, regexp = "Missing")
})

# Check for invalid dates
test_that("Check for invalid dates", {
  result1 <- get_site_MODIS(
          deimsid = "https://deims.org/86e3a1ca-b2ba-4b06-b096-71447df52841",
          earthdata_user = "abc",
          earthdata_passwd = "abc",
          from_date = "1999.01.01",
          to_date = "2002.30.03"
          )
  result2 <- get_site_MODIS(
          deimsid = "https://deims.org/86e3a1ca-b2ba-4b06-b096-71447df52841",
          earthdata_user = "abc",
          earthdata_passwd = "abc",
          from_date = "2017.01.01",
          to_date = "2017.13.03"
          )
  result3 <- get_site_MODIS(
          deimsid = "https://deims.org/86e3a1ca-b2ba-4b06-b096-71447df52841",
          earthdata_user = "abc",
          earthdata_passwd = "abc",
          from_date = "2017.02.01",
          to_date = "2017.02.30"
          )
  result4 <- get_site_MODIS(
          deimsid = "https://deims.org/86e3a1ca-b2ba-4b06-b096-71447df52841",
          earthdata_user = "abc",
          earthdata_passwd = "abc",
          from_date = "2017-02-01",
          to_date = "2017.02.20"
          )
  result5 <- get_site_MODIS(
          deimsid = "https://deims.org/86e3a1ca-b2ba-4b06-b096-71447df52841",
          earthdata_user = "abc",
          earthdata_passwd = "abc",
          from_date = "2018.03.01",
          to_date = "2018.01.01"
          )

  expect_error(result1, regexp = "Please check dates and format as: YYYY.mm.dd")
  expect_error(result2, regexp = "Please check dates and format as: YYYY.mm.dd")
  expect_error(result3, regexp = "Please check dates and format as: YYYY.mm.dd")
  expect_error(result4, regexp = "Please check dates and format as: YYYY.mm.dd")
  expect_error(result5, regexp = "Please check dates and format as: YYYY.mm.dd")
})

# Test for successful download, function returns terra::SpatRaster
test_that("After successfull download, Function returns SpatRaster", {
  ds <- get_site_MODIS(
          deimsid = "https://deims.org/86e3a1ca-b2ba-4b06-b096-71447df52841",
          # TODO: Setup auth credentials for this package
          earthdata_user = "",
          earthdata_passwd = "",
          product = "Vegetation_Indexes_Monthly_1Km (M*D13A3)",
          bands = "NDVI",
          from_date = "2017.01.01",
          to_date = "2017.06.30"
          )
  expect_s4_class(ds, "SpatRaster")
  # In this case, with both Aqua and Terra platforms, 
  # and datasets for 6 months, should be 12 layers
  expect_equal(terra::nlyr(ds), 12)
})
