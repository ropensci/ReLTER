message("\n---- Test get_site_info() ----")

skip_if_offline(host = "deims.org")

# 1. Test internet connection error
test_that("Expect error if internet connection is down", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_error(
    httptest2::without_internet(
      result <- ReLTER::get_site_info(
        deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
        show_map = FALSE
      )
    ),
    "GET"
  )
})

test_that("Wrong input (URL) returns message", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_message(
    object <- ReLTER::get_site_info(
      deimsid = "https://deims.org/site/invalidid",
      show_map = FALSE
    ),
    regexp = "The requested page could not be found"
  )
  expect_true(is.null(object$data))
})

test_that("Wrong input (not URL) returns message", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_message(
    object <- ReLTER::get_site_info(
      deimsid = "invalidid",
      show_map = FALSE
    ),
    regexp = "The requested page could not be found"
  )
  expect_true(is.null(object$data))
})

test_that("Site info adds requested categories correctly", {
  categories <- c("Affiliations", "Contacts", "EnvCharacts")
  result <- suppressWarnings(
    ReLTER::get_site_info(
      deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
      categories = categories,
      show_map = FALSE
    )
  )
  
  # Check that columns from categories are present
  expect_true(any(grepl("networks", names(result))))
  expect_true(any(grepl("metadataProvider", names(result))))
  expect_true(any(grepl("vegetation", names(result))))
})

test_that("Site info returns valid geometries if boundaries exist", {
  result <- suppressWarnings(
    ReLTER::get_site_info(
      deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
      show_map = FALSE
    )
  )
  if (inherits(result, "sf")) {
    expect_true(all(sf::st_is_valid(result)))
  }
})

test_that("Site without geometry returns tibble and NULL map", {
  result <- suppressWarnings(
    ReLTER::get_site_info(
      deimsid = "https://deims.org/b72a0976-15c0-4c8e-9254-b3bcbb42f95f",
      show_map = FALSE
    )
  )
  
  # It should be a tibble, not sf
  expect_s3_class(result, "tbl_df")
  expect_false(inherits(result, "sf"))
  
  # Expected columns present
  expected_cols <- c(
    "title", "uri", "country",
    "geoElev.avg", "geoElev.min", "geoElev.max",
    "geoElev.unit", "created", "changed"
  )
  expect_true(all(expected_cols %in% names(result)))
})
