message("\n---- Test get_site_MODIS() ----")
library(testthat)
#skip_on_cran()

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_ilter_generalinfo()
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})


#skip_if_offline(host = "deims.org")
#skip_if(skip_in_test_mode)

test_that("product is not supported",
      {result <- ReLTER::get_site_MODIS(product = "xyz")
      expect_error(result,  regexpr("xyz not supported"))
      })

test_that("non-existant site or no boundary polygon",
      {result <- ReLTER::get_site_MODIS(
        deimsid = "xyz",
        product = "VI",
        from_date = "2018.03.01", to_date = "2018.08.30",
        output_dir = tempdir(), output_proj = "3035")
      expect_error(result,
                   regexpr("No boundary for requested DEIMS site."))
      })

test_that("missing login creds for EarthData website",
      {# Unset earthdata_user env variable and test
        Sys.setenv(earthdata_user="")
        result <- ReLTER::get_site_MODIS(
          deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
          product = "VI",
          from_date = "2018.03.01", to_date = "2018.08.30",
          output_dir = tempdir(), output_proj = "3035")
      expect_error(result,
                   regexpr("No login credentials for EarthData."))
      # Restore earthdata_user env variable
      })

test_that("successful acquisition",
      # This test takes ~10 mins.
      { Sys.setenv("earthdata_user" = "mstp_test")
        Sys.setenv("earthdata_pass" = "MSTP_test_01")
        result <- ReLTER::get_site_MODIS(
          deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
          product = "VI",
          from_date = "2018.03.04", to_date = "2018.03.20",
          output_dir = tempdir(), output_proj = "3035",
          plot_ts=FALSE, show_map=FALSE)
        
        expect_type(result, "character")
        expect_length(result, 4)
        Sys.setenv(earthdata_user="")
        Sys.setenv(earthdata_pass="")
      })


