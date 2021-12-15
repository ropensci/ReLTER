message("\n---- Test get_dataset_info() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER::get_dataset_info(
        datasetid =
          "https://deims.org/dataset/38d604ef-decb-4d67-8ac3-cc843d10d3ef"
      )
    ),
    "GET"
  )
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of dataset function constructs ‘tibble’ as expected", {
  result <- ReLTER::get_dataset_info(
    datasetid =
      "https://deims.org/dataset/38d604ef-decb-4d67-8ac3-cc843d10d3ef"
  )
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 34)
  expect_true(all(names(result) == c(
    "title", "abstract", "keywords", "uri", "type",
    "dateRange.from", "dateRange.to", "relatedSite", "contacts.corresponding",
    "contacts.creator", "contacts.metadataProvider", "observationParameters",
    "observationSpecies", "dataPolicy", "doi", "onlineLocation",
    "legal.accessUse", "legal.rights", "legal.legalAct", "legal.citation",
    "method.instrumentation", "method.qualityAssurance", "method.methodUrl",
    "method.methodDescription", "method.samplingTimeUnit.label",
    "method.samplingTimeUnit.uri", "method.spatialDesign.label",
    "method.spatialDesign.uri", "method.spatialScale.label",
    "method.spatialScale.uri", "method.temporalResolution.label",
    "method.temporalResolution.uri", "boundaries", "boundariesDescription"
  )))

  expect_type(result$title, "character")
  expect_type(result$abstract, "character")
  expect_type(result$keywords, "list")
  expect_type(result$uri, "character")
  expect_type(result$type, "character")
  expect_type(result$dateRange.from, "character")
  expect_type(
    result$dateRange.to,
    "logical"
  ) # se fosse valorizzato sarebbe "character"
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
  expect_type(result$onlineLocation, "list")
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
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  result <- ReLTER::get_dataset_info(
    datasetid = "https://deims.org/dataset/ljhnhbkihubib"
  )
  expect_type(result, "NULL")
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  Sys.setenv('LOCAL_DEIMS' = FALSE) # set online mode
  result <- ReLTER::get_dataset_info(datasetid = "ljhnhbkihubib")
  expect_type(result, "NULL")
  Sys.setenv('LOCAL_DEIMS' = TRUE) # restore test mode
})

test_that("Output of get dataset information function constructs 'sf' with
          valid geometries", {
  result <- ReLTER::get_dataset_info(
    datasetid =
      "https://deims.org/dataset/38d604ef-decb-4d67-8ac3-cc843d10d3ef"
  )
  result_valid <- sf::st_is_valid(result)
  expect_true(any(result_valid))
})
