message("\n---- Test produce_site_map() ----")

sitesNetwork <- ReLTER::get_network_sites(
  networkDEIMSID =
    "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
)
# In the case of Italian sites are selected only true sites and excluded the
# macrosites.
sitesNetwork <- (sitesNetwork[!grepl("^IT", sitesNetwork$title), ])
sf::st_crs(sitesNetwork) <- 4326

test_that("Expect error if internet connection is down", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::produce_site_map(
        deimsid = TESTURLSite,
        countryCode = "ITA",
        listOfSites = sitesNetwork,
        gridNx = 0.7,
        gridNy = 0.35
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of site map function constructs 'list' as expected", {
            result <- ReLTER::produce_site_map(
              deimsid = TESTURLSite,
              countryCode = "ITA",
              listOfSites = sitesNetwork,
              gridNx = 0.7,
              gridNy = 0.35,
              show_map = TRUE
            )
            expect_type(result, "list")
          })

test_that("Output of site map function constructs 'tmap' as expected", {
            result <- ReLTER::produce_site_map(
              deimsid = TESTURLSite,
              countryCode = "ITA",
              listOfSites = sitesNetwork,
              gridNx = 0.7,
              gridNy = 0.35
            )
            expect_s3_class(result, "tmap")
          })
