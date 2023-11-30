message("\n---- Test get_site_ODS() ----")

skip_if_offline(host = "s3.eu-central-1.wasabisys.com")

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_site_info(
        deimsid = TESTURLSite,
        category = "Boundaries"
      )
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

skip_if_offline(host = "deims.org")

datasets <- c("landcover", "clc2018", "osm_buildings", "natura2000",
              "ndvi_spring", "ndvi_summer", "ndvi_autumn", "ndvi_winter")
deimsids <- c(
  # Hillsborough, UK
  "https://deims.org/371c5259-6f38-4aa7-9517-c56f608c62cc",
  # Cairngorm
  #"https://deims.org/1b94503d-285c-4028-a3db-bc78e31dea07",
  # Mondsee Austia, *No boundary*
  "https://deims.org/8a313716-ceed-4f41-8b0b-a8197bfc304a",
  # Zone Atelier Alps
  # Skip this site since it is too big
  #"https://deims.org/79d6c1df-570f-455f-a929-6cfe5c4ca1e9",
  # Sikfokut, Hungary
  "https://deims.org/632895f6-b954-4fd9-90bb-b427b22585ac"
)

for (id in seq_along(deimsids)) {
      if (id == 2) {
        # Mondsee, no boundary available on DEIMS, func should return NULL
        test_that("Function correctly returns NULL for site with no boundary", {
          ds <- ReLTER::get_site_ODS(
            deimsid = deimsids[id]
          ) # Default dataset is "landcover"
          expect_null(ds)
        })
        next # Don"t continue testing with this site
      } else {
      # Check other sites with all datasets
      for (s in seq_along(datasets)) {
          test_that("Function returns a SpatRaster", {
            ds <- get_site_ODS(deimsid = deimsids[id], datasets[s])
            expect_s4_class(ds, "SpatRaster")
          })
        }
      }
}

test_that("full_url with 'unavailable' value", {
  result <- ReLTER::get_site_ODS(
    deimsid = "https://deims.org/d0a8da18-0881-4ebe-bccf-bc4cb4e25701",
    dataset = TRUE
  )
  expect_null(result)
})

test_that("Wrong URL", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER::get_site_ODS(
    deimsid = "https://wrong.url",
    dataset = "osm_buildings"
  )
  expect_null(result)
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})
