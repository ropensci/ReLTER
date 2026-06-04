message("\n---- Test get_site_speciesOccurrences() ----")

ELTER_COLS <- c(
  "eLTER_title.x", "eLTER_uri", "eLTER_created", "eLTER_changed",
  "eLTER_geoCoord", "eLTER_country", "eLTER_geoElev.avg",
  "eLTER_geoElev.min", "eLTER_geoElev.max",
  "eLTER_biogeographicalRegion", "eLTER_biome", "eLTER_ecosystemType",
  "eLTER_eunisHabitat", "eLTER_landforms", "eLTER_geoBonBiome"
)

test_that("Expect error if internet connection is down", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_error(
    httptest2::without_internet(
      ReLTER::get_site_speciesOccurrences(
        deimsid  = TESTURLSite,
        list_DS  = "inat",
        show_map = FALSE,
        limit    = 10
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output is a list with map element always present", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- suppressMessages(
    ReLTER::get_site_speciesOccurrences(
      deimsid  = TESTURLSite,
      list_DS  = c("gbif", "inat", "obis"),
      show_map = FALSE,
      limit    = 10
    )
  )
  expect_type(result, "list")
  expect_true("map" %in% names(result))
  expect_s3_class(result$map, "leaflet")
})

test_that("show_map = FALSE does not print but map is still returned", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- suppressMessages(
    ReLTER::get_site_speciesOccurrences(
      deimsid  = TESTURLSite,
      list_DS  = c("gbif", "inat", "obis"),
      show_map = FALSE,
      limit    = 10
    )
  )
  expect_s3_class(result$map, "leaflet")
})

test_that("Source slots with results are sf objects with eLTER columns", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- suppressMessages(
    ReLTER::get_site_speciesOccurrences(
      deimsid  = TESTURLSite,
      list_DS  = c("gbif", "inat", "obis"),
      show_map = FALSE,
      limit    = 10
    )
  )
  # Check only sources that returned data
  sources_with_data <- names(result)[names(result) != "map"]
  if (length(sources_with_data) == 0L) {
    skip("No occurrences found within boundary for any source")
  }
  for (src in sources_with_data) {
    expect_s3_class(result[[src]], "sf")
    # eLTER columns present
    present_elter <- intersect(ELTER_COLS, names(result[[src]]))
    expect_true(length(present_elter) > 0)
    # Original source columns not dropped
    non_elter <- names(result[[src]])[!grepl("^eLTER_", names(result[[src]]))]
    expect_true(length(non_elter) > 0)
  }
})

test_that("Occurrences are within the site boundary in CRS 4326", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- suppressMessages(
    ReLTER::get_site_speciesOccurrences(
      deimsid  = TESTURLSite,
      list_DS  = c("gbif", "inat", "obis"),
      show_map = FALSE,
      limit    = 10
    )
  )
  sources_with_data <- names(result)[names(result) != "map"]
  if (length(sources_with_data) == 0L) {
    skip("No occurrences found within boundary for any source")
  }
  for (src in sources_with_data) {
    expect_equal(sf::st_crs(result[[src]])$epsg, 4326)
  }
})

test_that("exclude_inat_from_gbif removes iNaturalist records from GBIF", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- suppressMessages(
    ReLTER::get_site_speciesOccurrences(
      deimsid                = TESTURLSite,
      list_DS                = c("gbif", "inat"),
      show_map               = FALSE,
      limit                  = 50,
      exclude_inat_from_gbif = TRUE
    )
  )
  if (is.null(result$gbif)) {
    skip("No GBIF occurrences found within boundary for this site")
  }
  expect_false(any(result$gbif$institutionCode == "iNaturalist", na.rm = TRUE))
})

test_that("Wrong DEIMS ID (valid URL format) returns NULL", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- suppressMessages(
    ReLTER::get_site_speciesOccurrences(
      deimsid  = "https://deims.org/ljhnhbkihubib",
      list_DS  = c("gbif", "inat", "obis"),
      show_map = FALSE,
      limit    = 10
    )
  )
  expect_null(result)
})

test_that("Wrong DEIMS ID (not a URL) returns NULL", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- suppressMessages(
    ReLTER::get_site_speciesOccurrences(
      deimsid  = "ljhnhbkihubib",
      list_DS  = c("gbif", "inat", "obis"),
      show_map = FALSE,
      limit    = 10
    )
  )
  expect_null(result)
})