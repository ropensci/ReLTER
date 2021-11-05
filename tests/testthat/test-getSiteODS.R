message("\n---- Test getSiteODS() ----")

library(testthat)
test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::getSiteEnvCharacts(deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe')
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

datasets <- c("landcover", "clc2018", "osm_buildings", "natura2000",
              "ndvi_spring", "ndvi_summer", "ndvi_autumn", "ndvi_winter")
deimsids <- c("https://deims.org/1b94503d-285c-4028-a3db-bc78e31dea07", # Cairngorm
              "https://deims.org/8a313716-ceed-4f41-8b0b-a8197bfc304a", # Mondsee Austia, *No boundary*
              "https://deims.org/79d6c1df-570f-455f-a929-6cfe5c4ca1e9", # Zone Atelier Alps
              "https://deims.org/632895f6-b954-4fd9-90bb-b427b22585ac", # Sikfokut, Hungary
            )

for (d in 1:length(demisids) {
      if (d == 2) {  #Mondsee, no boundary available on DEIMS, func should return NULL
        test_that("Function correctly returns NULL for site with no boundary", {
          ds <- getSiteODS(deimsid = deimsids[id]) # Default dataset is "landcover"
          expect_null(ds)
        })
        break     # Don't continue testing with this site
      } else {
      # Check other sites with all datasets
      for (s in 1:length(datasets) {    
          test_that("Function returns a SpatRaster", {
            ds <- getSiteODS(deimsid = deimsids[d], datasets[s])
            expect_s4_class(ds, "SpatRaster")
          })
        }
      }
}
