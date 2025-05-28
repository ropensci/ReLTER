message("\n---- Test get_site_boundaries() ----")

skip_if_offline(host = "deims.org")

test_that("Expect error if internet connection is down", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_error(
    httptest2::without_internet(
      result <- ReLTER::get_site_boundaries(
        deimsid = TESTURLSite
      )
    ),
    "GET"
  )
})

# test_that("Output of site boundaries function constructs 'sf' and 'tibble' as
#           expected", {
#   result <- ReLTER:::get_site_boundaries(
#     deimsid = TESTURLSite
#   )
#   expect_s3_class(result, "sf")
#   expect_s3_class(result, "data.frame")
#   expect_true(ncol(result) == 3)
#   expect_true(all(names(result) == c(
#     "title", "uri", "geometry"
#   )))
#   expect_type(result$title, "character")
#   expect_type(result$uri, "character")
#   if (is.null(result$boundaries)) {
#     expect_type(result$boundaries, "NULL")
#   } else {
#     expect_type(result$boundaries, "list")
#   }
# })
# 

test_that("Wrong input (but URL) constructs a NULL object", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER::get_site_boundaries(
    deimsid = "https://deims.org/ljhnhbkihubib"
  )
  expect_type(result, "NULL")
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER::get_site_boundaries(deimsid = "ljhnhbkihubib")
  expect_type(result, "NULL")
})

test_that("get_site_boundaries with locations=TRUE and correct deimsid return list with names data, map, locations", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- get_site_boundaries(
      deimsid = "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",
     show_map = FALSE,
     with_locations = TRUE
    )
  expect_true(length(names(result)) > 0)
  expect_true(all(c("data", "map", "locations") %in% names(result)))
})

test_that("get_site_boundaries with locations=FALSE and correct deimsid return list with names data, map, locations", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- get_site_boundaries(
    deimsid = "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6",
    show_map = FALSE,
    with_locations = FALSE
  )
  expect_true(length(names(result)) > 0)
  expect_true(all(c("data", "map", "locations") %in% names(result)))
})
  