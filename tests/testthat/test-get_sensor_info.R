message("\n---- Test get_sensor_info() ----")

test_that("Expect error if internet connection is down", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_error(
    httptest::without_internet(
      result <- ReLTER::get_sensor_info(
        sensorid =
          "https://deims.org/sensors/3845475c-4aec-4dd7-83b4-0ab6ba95db35",
        show_map = FALSE
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of sensor information function constructs 'sf' and
          'tibble' as expected", {
            object <- ReLTER::get_sensor_info(
              sensorid =
                "https://deims.org/sensors/3845475c-4aec-4dd7-83b4-0ab6ba95db35",
              show_map = FALSE
            )
            expect_s3_class(object, "sf")
            expect_s3_class(object, "tbl_df")
            expect_true(ncol(object) == 14)
            expect_true(all(names(object) == c(
              "title", "abstract", "uri", "type", "created", "changed",
              "relatedSite", "contacts", "geography",
              "elevation", "sensorType.label",
              "sensorType.uri", "resultAcquisitionSource", "observedProperty"
            )))
            expect_type(object$title, "character")
            expect_type(object$geography, "list")
          })

test_that("Wrong input (but URL) constructs a NULL object", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_error(
    object <- ReLTER::get_sensor_info(
      sensorid = "https://deims.org/activity/ljhnhbkihubib",
      show_map = FALSE
    ),
    regexp = "Page Not Found"
  )
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  expect_error(
    object = ReLTER::get_sensor_info(
      sensorid = "ljhnhbkihubib",
      show_map = FALSE
    ),
    regexp = "Page Not Found"
  )
})

test_that("Output of get sensor information function constructs 'sf' with
          valid geometries", {
            result <- ReLTER::get_sensor_info(
              sensorid =
                "https://deims.org/sensors/3845475c-4aec-4dd7-83b4-0ab6ba95db35",
              show_map = FALSE
            )
            result_valid <- sf::st_is_valid(result)
            expect_true(any(result_valid))
          })
