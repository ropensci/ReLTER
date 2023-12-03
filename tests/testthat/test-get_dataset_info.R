message("\n---- Test get_dataset_info() ----")

test_that("Expect error if internet connection is down", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_dataset_info(
        datasetid =
          "https://deims.org/dataset/38d604ef-decb-4d67-8ac3-cc843d10d3ef"
      )
    ),
    "GET"
  )
})

skip_if_offline(host = "deims.org")

test_that("Output of dataset function constructs ‘tibble’ as expected", {
  result <- ReLTER::get_dataset_info(
    datasetid =
      "https://deims.org/dataset/38d604ef-decb-4d67-8ac3-cc843d10d3ef",
    show_map = FALSE
  )
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 41)
  expect_true(all(names(result) == c(
    "title", "abstract", "keywords", "uri",
    "type", "created", "changed", "dateRange.from",
    "dateRange.to", "relatedSite", "contacts.corresponding", "contacts.creator",
    "contacts.metadataProvider", "observationParameters", "observationSpecies", "dataPolicy",
    "doi", "onlineDistribution.dataPolicyUrl", "onlineDistribution.doi",
    "onlineDistribution.onlineLocation", "legal.accessUse", "legal.rights", "legal.legalAct",
    "legal.citation", "method.instrumentation", "method.qualityAssurance", "method.methodUrl",
    "method.methodDescription", "method.samplingTimeUnit.label", "method.samplingTimeUnit.uri",
    "method.spatialDesign.label", "method.spatialDesign.uri", "method.spatialScale.label",
    "method.spatialScale.uri", "method.temporalResolution.label", "method.temporalResolution.uri",
    "boundaries", "boundariesDescription", "elevation.min", "elevation.max",
    "elevation.unit"
  )))
  expect_type(result$title, "character")
  expect_type(result$abstract, "character")
  expect_type(result$keywords, "list")
  expect_type(result$uri, "character")
  expect_type(result$type, "character")
  expect_type(result$relatedSite, "list")
  expect_type(result$contacts.corresponding, "list")
  expect_type(result$contacts.creator, "list")
  expect_type(
    result$contacts.metadataProvider,
    "logical"
  ) # se fosse valorizzato sarebbe "list"
  expect_type(result$observationParameters, "list")
  expect_type(result$observationSpecies, "list")
  expect_type(result$dataPolicy, "list")
  expect_type(result$doi, "character")
  expect_type(result$onlineDistribution.onlineLocation, "list")
  expect_type(result$legal.accessUse, "list")
  expect_type(
    result$legal.rights,
    "logical"
  ) # se fosse valorizzato sarebbe "list"
  expect_type(
    result$legal.legalAct,
    "logical"
  ) # se fosse valorizzato sarebbe "list"
  expect_type(
    result$legal.citation,
    "logical"
  ) # se fosse valorizzato sarebbe "list"
  expect_type(
    result$method.instrumentation,
    "logical"
  ) # se fosse valorizzato sarebbe "list"
  expect_type(
    result$method.qualityAssurance,
    "logical"
  ) # se fosse valorizzato sarebbe "?"
  expect_type(result$method.methodUrl, "list")
  expect_type(result$method.methodDescription, "list")
  expect_type(result$method.samplingTimeUnit.label, "character")
  expect_type(
    result$method.samplingTimeUnit.uri,
    "logical"
  ) # se fosse valorizzato sarebbe "character"
  expect_type(result$method.spatialDesign.label, "character")
  expect_type(
    result$method.spatialDesign.uri,
    "logical"
  ) # se fosse valorizzato sarebbe "character"
  expect_type(result$method.spatialScale.label, "character")
  expect_type(
    result$method.spatialScale.uri,
    "logical"
  ) # se fosse valorizzato sarebbe "character"
  expect_type(result$method.temporalResolution.label, "character")
  expect_type(
    result$method.temporalResolution.uri,
    "logical"
  ) # se fosse valorizzato sarebbe "character"
  expect_type(
    result$boundaries, "list")
  expect_type(
    result$boundariesDescription,
    "character"
  )
})

test_that("Wrong input (but URL) constructs a NULL object", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER::get_dataset_info(
    datasetid = "https://deims.org/dataset/ljhnhbkihubib",
    show_map = FALSE
  )
  expect_type(result, "NULL")
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  withr::local_envvar("LOCAL_DEIMS" = FALSE)
  result <- ReLTER::get_dataset_info(
    datasetid = "ljhnhbkihubib",
    show_map = FALSE
  )
  expect_type(result, "NULL")
})

test_that("Output of get dataset information function constructs 'sf' with
          valid geometries", {
  result <- ReLTER::get_dataset_info(
    datasetid =
      "https://deims.org/dataset/38d604ef-decb-4d67-8ac3-cc843d10d3ef",
    show_map = FALSE
  )
  result_valid <- sf::st_is_valid(result)
  expect_true(any(result_valid))
})

test_that("Verify that 'observationParameters' is NULL", {
  result <- ReLTER::get_dataset_info(
    datasetid =
      "https://deims.org/dataset/3cd76d66-cadc-4d10-9fa7-75fe8d60663c",
    show_map = FALSE
  )
  expect_equal(
    result$observationParameters[[1]]$parametersLabel,
    NA
  )
  expect_equal(
    result$observationParameters[[1]]$parametersUri,
    NA
  )
})

test_that("Verify that 'observationSpecies' is NULL", {
  result <- ReLTER::get_dataset_info(
    datasetid =
      "https://deims.org/dataset/3cd76d66-cadc-4d10-9fa7-75fe8d60663c",
    show_map = FALSE
  )
  expect_type(
    result$observationSpecies[[1]]$parametersLabel,
    "NULL"
  )
  expect_equal(
    any(result$observationSpecies[[1]]$speciesUri),
    NA
  )
})
