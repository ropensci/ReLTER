library(httptest)
TESTURLNetwork <-
  "https://deims.org/networks/e0f680c2-22b1-4424-bf54-58aa9b7476a0"
TESTURLSite <- "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"

# define test mode if...
test_mode <- if (Sys.getenv("LOCAL_DEIMS") == "") {
  any(
    isTRUE(as.logical(Sys.getenv("CI"))),
    !identical(Sys.getenv("NOT_CRAN"), "true")
  )} else {
    Sys.getenv("LOCAL_DEIMS")
  }
skip_in_test_mode <- test_mode
  
# # overwrite manually to test "test mode" (this should be normally commented!)
# test_mode <- TRUE
# skip_in_test_mode <- FALSE

Sys.setenv("LOCAL_DEIMS" = test_mode) # run get_id(..., test = TRUE)
