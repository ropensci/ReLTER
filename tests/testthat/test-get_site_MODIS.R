message("\n---- Test get_site_MODIS() ----")

library(testthat)
skip_if_offline(host = "deims.org")
skip_if_offline(host = "e4ftl01.cr.usgs.gov")

# Check for requested products or bands list (function returns NULL)
test_that("When show_products is TRUE, or show_bands is character,
          result is NULL", {
  result1 <- get_site_MODIS(show_products = TRUE)
  result2 <- get_site_MODIS(show_bands = "M*D13Q1")
  expect_type(result1, "NULL")
  expect_type(result2, "NULL")
})

# Check for password and username
test_that("Username/Password are character string", {
          expect_error(get_site_MODIS(
          deimsid = "https://deims.org/45722713-80e3-4387-a47b-82c97a6ef62b",
          earthdata_user = "abc",
          earthdata_passwd = NULL,
          from_date = "2017.01.01",
          to_date = "2017.30.03"), NULL)

          expect_error(get_site_MODIS(
          deimsid = "https://deims.org/45722713-80e3-4387-a47b-82c97a6ef62b",
          earthdata_user = NULL,
          earthdata_passwd = "abc",
          from_date = "2017.01.01",
          to_date = "2017.30.03"), NULL)
})

# Check for invalid dates
test_that("Check for invalid dates", {
  expect_error(get_site_MODIS(
          deimsid = "https://deims.org/45722713-80e3-4387-a47b-82c97a6ef62b",
          earthdata_user = "abc",
          earthdata_passwd = "abc",
          from_date = "1999.01.01",
          to_date = "2002.30.03"), NULL)
  expect_error(get_site_MODIS(
          deimsid = "https://deims.org/45722713-80e3-4387-a47b-82c97a6ef62b",
          earthdata_user = "abc",
          earthdata_passwd = "abc",
          from_date = "2017.01.01",
          to_date = "2017.13.03"), NULL)
  expect_error(get_site_MODIS(
          deimsid = "https://deims.org/45722713-80e3-4387-a47b-82c97a6ef62b",
          earthdata_user = "abc",
          earthdata_passwd = "abc",
          from_date = "2017.02.01",
          to_date = "2017.02.30"), NULL)
  expect_error(get_site_MODIS(
          deimsid = "https://deims.org/45722713-80e3-4387-a47b-82c97a6ef62b",
          earthdata_user = "abc",
          earthdata_passwd = "abc",
          from_date = "2017-02-01",
          to_date = "2017.02.20"), NULL)
  expect_error(get_site_MODIS(
          deimsid = "https://deims.org/45722713-80e3-4387-a47b-82c97a6ef62b",
          earthdata_user = "abc",
          earthdata_passwd = "abc",
          from_date = "2018.03.01",
          to_date = "2018.01.01"), NULL)
})

# Test for successful download, function returns terra::SpatRaster
test_that("After successfull download, Function returns SpatRaster", {
  # Load username and password from secrets
  creds <- list(
    "user" = Sys.getenv("USGS_USER"),
    "password" = Sys.getenv("USGS_PASSWORD")
  )
  ds <- try(get_site_MODIS(
          deimsid = "https://deims.org/45722713-80e3-4387-a47b-82c97a6ef62b",
          earthdata_user = creds$user,
          earthdata_passwd = creds$password,
          product = "Vegetation_Indexes_Monthly_1Km (M*D13A3)",
          bands = "NDVI",
          from_date = "2017.01.01",
          to_date = "2017.04.30"
          ))
  if (!is.null(ds) & !inherits(ds, "try-error")) {
  expect_s4_class(ds, "SpatRaster")
  # In this case, with both Aqua and Terra platforms, 
  # and datasets for 4 months, should be 8 layers (Aqua and Terra)
  expect_equal(terra::nlyr(ds), 8)
  }
})
