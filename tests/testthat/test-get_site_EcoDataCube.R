message("\n---- Test get_site_EcoDataCube() ----")

skip_if_offline(host = "deims.org")

# test_that("Expect error if internet connection is down", {
#   withr::local_envvar("LOCAL_DEIMS" = FALSE)
#   expect_error(
#     httptest2::without_internet(
#       result <- get_site_boundaries(
#         deimsid = TESTURLSite
#       )
#     ),
#     "GET"
#   )
# })

test_that("Output is SpatRaster", {
 result <- ReLTER:::get_site_EcoDataCube(
   deimsid = TESTURLSite,
   dataset = "MODIS_LST_day",
   dataset_month = "06",
   dataset_year = "2020",
   show_map = FALSE
 )
 expect_s4_class(result, "SpatRaster")
})


test_that("Wrong dataset_year returns NULL", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER:::get_site_EcoDataCube(
    deimsid = TESTURLSite,
    dataset = "MODIS_LST_day",
    dataset_month = "06",
    dataset_year = "1999",
    show_map = FALSE
  )
  expect_type(result, "NULL")
})

test_that("Wrong dataset_month returns NULL", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER:::get_site_EcoDataCube(
    deimsid = TESTURLSite,
    dataset = "MODIS_LST_day",
    dataset_month = "13",
    dataset_year = "2010",
    show_map = FALSE
  )
  expect_type(result, "NULL")
})

