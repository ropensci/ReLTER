#' https://docs.google.com/spreadsheets/d/1wTiwb_5uM_XGsrSWUx9h2t4maSSFg7mpggq2nIydWco
#'
#' `eLTER_data_reporting_format` data is a nested `list` describing the field
#' specifications used in the eLTER data reporting process.
#' It includes two versions of the format: version 1.3 (used in eLTER-Plus) and version 2.0 
#' (proposed update). Each version contains multiple components such as DATA, REFERENCE, 
#' METHOD, STATION, EVENT, and SAMPLE, which define the expected fields for each section.
#'
#' @format A named list with two main elements:
#' \describe{
#'   \item{version1.3}{Field names grouped by section as used in eLTER-Plus. Each section 
#'   (e.g. DATA, METHOD, STATION) contains field names for core and extended data structures.}
#'   \item{version2.0}{Updated field names grouped by section, reflecting refinements in naming conventions and structure.}
#' }
#'
#' @source eLTER-Plus field specification document: 
#' \url{https://doi.org/10.5281/zenodo.6373410}  
#' Proposed updated format: 
#' \url{https://docs.google.com/spreadsheets/d/1wTiwb_5uM_XGsrSWUx9h2t4maSSFg7mpggq2nIydWco}
#' 
#' @usage data(eLTER_data_reporting_format)
"eLTER_data_reporting_format"