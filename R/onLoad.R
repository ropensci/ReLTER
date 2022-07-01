.onLoad <- function(libname, pkgname) {
  # add here code to check deims api version and consequently load jq queries.
  sys_url <- Sys.getenv("ReLTER_deimsBaseURL")
  if (!is.null(sys_url) && sys_url != "") {
    set_deims_base_url(sys_url)
  }
}

# NOTE: to set the sys_url use:
# usethis::edit_r_environ("project")
# and add line ReLTER_deimsBaseURL=<deims url>
