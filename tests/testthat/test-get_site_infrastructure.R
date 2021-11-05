message("\n---- Test get_site_infrastructure() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_site_infrastructure(deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe')
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of site infrastructure function constructs ‘tibble’ as expected", {
  result <- ReLTER::get_site_infrastructure(deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe')
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 21)
  expect_true(all(names(result) == c(
    "title", "uri", "geoCoord", "country",
    "geoElev.avg", "geoElev.min", "geoElev.max", "geoElev.unit",
    "generalInfo.accessibleAllYear", "generalInfo.accessType", "generalInfo.allPartsAccessible", "generalInfo.maintenanceInterval",
    "generalInfo.permanentPowerSupply", "generalInfo.operation.permanent", "generalInfo.operation.notes", "generalInfo.operation.siteVisitInterval", "generalInfo.notes", "generalInfo.collection",
    "generalInfo.data.policy.url", "generalInfo.data.policy.rights", "generalInfo.data.policy.notes"
  )))
  
  # TODO: alcune volte il tipo che viene prodotto è diverso da quello che effettivamente dovrebbe essere
  #   perché all'origine (nel JSON) il campo è vuoto. Come fare? Si forza il valore nella funzione o
  #   si può testare un doppio tipo?
  
  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  expect_type(result$geoCoord, "character")
  expect_type(result$country, "list")
  expect_type(result$geoElev.avg, "double")
  expect_type(result$geoElev.min, "integer")
  expect_type(result$geoElev.max, "integer")
  expect_type(result$geoElev.unit, "character")
  
  expect_type(result$generalInfo.accessibleAllYear, "logical")
  expect_type(result$generalInfo.accessType, "character")
  expect_type(result$generalInfo.allPartsAccessible, "logical")
  expect_type(result$generalInfo.maintenanceInterval, "integer")
  expect_type(result$generalInfo.permanentPowerSupply, "logical")
  expect_type(result$generalInfo.operation.permanent, "logical")
  expect_type(result$generalInfo.operation.notes, "logical")
  expect_type(result$generalInfo.operation.siteVisitInterval, "integer")
  expect_type(result$generalInfo.notes, "logical")
  expect_type(result$generalInfo.collection, "list")
  expect_type(result$generalInfo.data.policy.url, "logical")
  expect_type(result$generalInfo.data.policy.rights, "list")
  expect_type(result$generalInfo.data.policy.notes, "logical")
  # expect_true(sf::st_distance(
  #   sf::st_as_sfc(result$geoCoord, crs = 4326), 
  #   sf::st_sfc(sf::st_point(c(8.63403, 45.9547)), crs = 4326)
  # )[1,1] < 1e-4*units::as_units("m"))
})

test_that("Wrong input (but URL) constructs a tibble with empty data", {
  result <- ReLTER::get_site_infrastructure(deimsid = 'https://deims.org/ljhnhbkihubib')
  expect_type(result, "NULL")
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  result <- ReLTER::get_site_infrastructure(deimsid = 'ljhnhbkihubib')
  expect_type(result, "NULL")
})

