message("\n---- Test get_site_MODIS() ----")
library(testthat)
# skip_on_cran()


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

# Checks for legitimate parameters
test_that("product is not supported",
   {expect_error(ReLTER::get_site_MODIS(product = "xyz"),
                 "xyz not supported")
   })

test_that("missing login creds for EarthData website",
          {# Unset earthdata_user env variable and test
            Sys.setenv(earthdata_user="")
            
            expect_error(suppressWarnings(ReLTER::get_site_MODIS(
              deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
              product = "VI",
              from_date = "2018.03.01", to_date = "2018.08.30",
              output_dir = tempdir(), output_proj = "3035")),
              "No login credentials for EarthData.")
          })

# Online tests
skip_if_offline(host = "deims.org")

test_that("non-existant site",
   {Sys.setenv("LOCAL_DEIMS" = FALSE)
    expect_error(ReLTER::get_site_MODIS(
       deimsid = "https://deims.org/abcxyz",
       product = "VI",
       from_date = "2018.03.01", to_date = "2018.08.30",
       output_dir = tempdir(), output_proj = "3035"),
    "No boundary for requested DEIMS site.")
    Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode)
   })
 
test_that("site with no boundary polygon",
   {expect_error(ReLTER::get_site_MODIS(
            deimsid = "https://deims.org/52d25867-33e7-4f27-8e0c-4f8a74bf22e0",
            product = "VI",
            from_date = "2018.03.01", to_date = "2018.08.30",
            output_dir = tempdir(), output_proj = "3035"),
    "No boundary for requested DEIMS site.")
   })

 
test_that("successful acquisition",
   # This test takes ~10 mins.
   { Sys.setenv("earthdata_user" = "mstp_test")
     Sys.setenv("earthdata_pass" = "MSTP_test_01")
     expect_type(result <- ReLTER::get_site_MODIS(
           deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
           product = "VI",
           from_date = "2018.03.04", to_date = "2018.03.20",
           output_dir = tempdir(), output_proj = "3035",
           plot_ts=FALSE, show_map=FALSE),
           "character")
         
     expect_length(result, 4)
     Sys.setenv(earthdata_user="")
     Sys.setenv(earthdata_pass="")
   })
