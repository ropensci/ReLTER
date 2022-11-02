message("\n---- Test some eLTER_reporting functions ----")

library(testthat)

test_that("map_occ2 returns list with appropriate names", {
  id <- "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6" #zobelboden
  gres <- get_site_speciesOccurrences(id, list_DS = c("gbif", "inat"), show_map = F, 
                                      exclude_inat_from_gbif = F)
  
  res <- tibble::as_tibble(gres$gbif) %>% map_occ_gbif2elter(deimsid = id)
  expect_named(res, expected = c("deimsid", "source", "data_mapping", "reference_TAXA",
                                 "reference_VARIABLES"))
  
  res2 <- tibble::as_tibble(gres$inat) %>% map_occ_inat2elter(deimsid = id)
  expect_named(res2, expected = c("deimsid", "source", "data_mapping", "reference_TAXA",
                                 "reference_VARIABLES"))
  
  # TODO: check what happens when obis contains no data
  # TODO: investigate the warning raised by map_occ_obis2elter 
  # (Problem while computing `ABS_POSITION = paste0(...)`. NAs introduced by coercion )
  
  # obis case: use id of site containing obis observations
  suppressWarnings({
    govid<-"https://deims.org/758087d7-231f-4f07-bd7e-6922e0c283fd"
    gres_obis <- get_site_speciesOccurrences(deimsid = govid, list_DS = c("obis"), show_map = F)
    x <- tibble::as_tibble(gres_obis$obis) 
    res3 <- map_occ_obis2elter(x, deimsid = govid)
    expect_named(res3, expected = c("deimsid", "source", "data_mapping", "reference_TAXA",
                                    "reference_VARIABLES"))
  })
})

test_that("map_occ2 reference_VARIABLE slot table contains VARIABLE_CODE values present 
          within data_mapping slot table and reference_TAXA contains all and only its TAXA", {
  id <- "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6" #zobelboden
  gres <- get_site_speciesOccurrences(id, list_DS = c("gbif"), show_map = F, 
                                      exclude_inat_from_gbif = F)
  
  res <- tibble::as_tibble(gres$gbif) %>% map_occ_gbif2elter(deimsid = id)
  expect_true(all(res$reference_VARIABLES %>% 
        dplyr::select(VARIABLE_CODE) %>% 
        unique() %>% 
        dplyr::pull() %in% 
          res$data_mapping %>% names()))
  
  expect_equal(res$reference_TAXA %>% 
                 dplyr::select(CODE) %>% 
                 dplyr::arrange() %>% 
                 dplyr::pull(),
               res$data_mapping %>% 
                 dplyr::select(TAXA) %>% 
                 unique() %>% 
                 dplyr::arrange() %>% 
                 dplyr::pull())
})

test_that("save_occ_eLTER_reporting_Archive saves zip archive", {
  id <- "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6" #zobelboden
  gres <- get_site_speciesOccurrences(id, list_DS = c("gbif"), show_map = F, 
                                      exclude_inat_from_gbif = F)
  
  res <- tibble::as_tibble(gres$gbif) %>% map_occ_gbif2elter(deimsid = id)
  # if called with not-existent dir, it fails
  expect_error(save_occ_eLTER_reporting_Archive(res, path = paste0(tempdir(),"/dummy/dummy")))
  fout <- save_occ_eLTER_reporting_Archive(res)
  expect_true(file.exists(fout))
  file.remove(fout)
})

test_that("reporting_compose_file_name: raises error if no deimsid nor 
          country code and site name are given",{
  expect_error(
    reporting_compose_file_name(deimsid = NULL, 
                                country_code = NULL, 
                                site_name=NULL, 
                                data_topic = "test", time_span = "2007-08")
  )
})

# NOTE: please refactor this following suggestions from 
# https://testthat.r-lib.org/articles/test-fixtures.html
# and https://r-pkgs.org/testing-advanced.html
test_that("elter_reporting_produce_data_object outputs list with expected slot names", {
  deimsid <- "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6"
  time_span <- 2015 # e.g. whole year
  # time_span <- "20150302-20180415" # e.g. span between two dates
  data_topic <- "VEG" # data provider defined abbreviation of "vegetation"
  variable_group <- "SPECCOVER" # data provider defined abbreviation
  version <- "V20220907"

  filename <- reporting_compose_file_name(
    deimsid = deimsid,
    data_topic = data_topic,
    variable_group = variable_group,
    time_span = time_span,
    version = version
  )
  
  expect_type(filename, "character")

  data <- dplyr::tribble(
    ~id, ~value,
    1, 7.5,
    2, 4.2
  )
  station <- dplyr::tribble(
    ~SITE_CODE, ~STATION_CODE, ~STYPE, ~LAT,      ~LON,       ~ALTITUDE,
    deimsid,    "IP2",         "AREA",  45.340805, 7.88887495, 265
  )
  method <- dplyr::tribble(
    ~VARIABLE, ~METH_DESCR,
    "COVE_F",  "Analysis of ammonium..."
  )

  research_object <- reporting_produce_data_object_v1.3(
   filename = filename,
   deimsid = deimsid,
   data = data,
   station = station,
   method = method
  )
  
  expect_named(research_object, c("filename", "type", "deimsid",
                                  "DATA", "STATION", "METHOD",
                                  "REFERENCE", "EVENT", "SAMPLE",
                                  "LICENCE"))
})

# NOTE: please refactor this following suggestions from 
# https://testthat.r-lib.org/articles/test-fixtures.html
# and https://r-pkgs.org/testing-advanced.html
test_that("reporting_save_archive write zip file on disk", {
  deimsid <- "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6"
  time_span <- 2015 # e.g. whole year
  # time_span <- "20150302-20180415" # e.g. span between two dates
  data_topic <- "VEG" # data provider defined abbreviation of "vegetation"
  variable_group <- "SPECCOVER" # data provider defined abbreviation
  version <- "V20220907"
  
  filename <- reporting_compose_file_name(
    deimsid = deimsid,
    data_topic = data_topic,
    variable_group = variable_group,
    time_span = time_span,
    version = version
  )
  
  data <- dplyr::tribble(
    ~id, ~value,
    1, 7.5,
    2, 4.2
  )
  station <- dplyr::tribble(
    ~SITE_CODE, ~STATION_CODE, ~STYPE, ~LAT,      ~LON,       ~ALTITUDE,
    deimsid,    "IP2",         "AREA",  45.340805, 7.88887495, 265
  )
  method <- dplyr::tribble(
    ~VARIABLE, ~METH_DESCR,
    "COVE_F",  "Analysis of ammonium..."
  )
  
  research_object <- reporting_produce_data_object_v1.3(
    filename = filename,
    deimsid = deimsid,
    data = data,
    station = station,
    method = method
  )
  
  fpath<-tempdir()
  fname<-"test_archive"
  savedFiles<-reporting_save_archive(research_object, filename = fname, filepath = fpath, saveRDS = TRUE)
  expect_true(file.exists(savedFiles$zip))
  expect_true(file.exists(savedFiles$RDS))
  file.remove(c(savedFiles$zip, savedFiles$RDS))
})
