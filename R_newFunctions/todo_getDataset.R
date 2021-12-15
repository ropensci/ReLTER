# 1. eLTER site: https://deims.org/api/sites/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6

# 2. in the relatedResources the list of resources with prefix and suffix (e.g. prefix": "https://deims.org/dataset/","suffix": "cd1fb6f8-5e57-11e3-aa73-005056ab003f")

# 3. eLTER relatedResources: 

# 3.1 1st case: https://deims.org/api/datasets/cd1fb6f8-5e57-11e3-aa73-005056ab003f where the title is "B2Share Landing Page"
# ... onlineDistribution": {
#     "dataPolicyUrl": null,
#     "doi": null,
#     "onlineLocation": [
#         {
#             "function": "url",
#             "url": {
#                 "title": "B2Share Landing Page",
#                 "value": "https://b2share.eudat.eu/records/2ec9cd568cfd40aa899beadcc8fe1b86"
#             },
#             "email": null
#         }
#     ]
# }, ...

# ISSUE:
# a. for this dataset https://deims.org/dataset/de97b80c-8285-43d4-8ffa-1bea3671ed05 the landing page is the DOI of resources 
#    (http://doi.org/10.23728/b2share.5db216134fd348e98c67e76fbc12630a). By this link is not usefull for reach or retrive (see point 3.1.1) the JSON version of the B2Share page of dataset 
# b. if the repository of the data is different to b2share the "url$title" or the "landingPageURI" values must be changed in according with. 
#    The reference to the file must be defined in DEIMS-SDR withou any other steps!
# c. as a consequence of issue b - the "downloadLink", in the future, must be the unique valid link to reach the data!
# d. in the case of https://deims.org/dataset/38d604ef-decb-4d67-8ac3-cc843d10d3ef LTER Northern Adriatic Sea (Italy) marine data from 1965 to 2015 the ePIC_PID has 2 URI: i. the first point to the dataset
#    ii. the second to the txt file for metadata of the dataset
# SOLVED e. in the case of https://deims.org/dataset/38d604ef-decb-4d67-8ac3-cc843d10d3ef LTER Northern Adriatic Sea (Italy) marine data from 1965 to 2015 the file is has a CSV extension.
#    How to work with different extension without know before it? The b2share don't provide to a extension field.
# f. in the case of CSV I don't know the separation characters
# g. in the case of 'https://deims.org/dataset/cd1fb6f8-5e57-11e3-aa73-005056ab003f' LTER Zöbelboden, Austria, Air chemistry, 2012 the
#    xsl is comprensive of more than one tabs and is not possible to reach which is the one with the data.
# h. in the case of Zöbelboden site (e.g. 'https://deims.org/dataset/79cf34dd-0c01-4d67-809b-67416dfeccb0' LTER Zöbelboden, Austria, Foliage chemistry, 2019) the landing
#    page link is the DOI provided by B2Share and, following this, is not possible to reach the URI of the landing page of B2share where I can reach the file.
# i. in the case of all datasets provided by Mar Piccolo di Taranto site (e.g. 'https://deims.org/dataset/b7dfe900-9592-4154-9458-d99510b8e066') the metada cointain B2share DOI 
#    but is not in the right or same filed of the DEIMS-SDR API: is it in the "onlineDistribution" field of JSON but are written like this:
# onlineDistribution": {
# 
#     "dataPolicyUrl": null,
#     "doi": "http://doi.org/10.23728/b2share.d0f6225e78124be5b14562f90323e064",
#     "onlineLocation": null

# 3.1.1 the link https://b2share.eudat.eu/records/2ec9cd568cfd40aa899beadcc8fe1b86 is direct to the landingPage, adding "api" before "records" 
# (e.g. https://b2share.eudat.eu/api/records/2ec9cd568cfd40aa899beadcc8fe1b86) is possible to reach the JSON of B2share record
# in this record the link to the file:
# ... links": {
#     "files": "https://b2share.eudat.eu/api/files/ff39d40d-eb09-4632-a114-93dcb9f1f54a",
#     "self": "https://b2share.eudat.eu/api/records/2ec9cd568cfd40aa899beadcc8fe1b86",
#     "versions": "https://b2share.eudat.eu/api/records/529d270c17d6454b80ac17d1d27c71fe/versions"
# }, ...
# end eventualy to the DOI:
# ... metadata": {
#   "$schema": "https://b2share.eudat.eu/api/communities/d952913c-451e-4b5c-817e-d578dc8a4469/schemas/0#/json_schema",
#   "DOI": "http://doi.org/XXXX/b2share.2ec9cd568cfd40aa899beadcc8fe1b86", ...
# the DOI can be reached also in the eLTER dataset JSON record if presented or added from the manager after the uploading operation.
# 3.1.2 file can be downloaded combined the files uri (https://b2share.eudat.eu/api/files/ff39d40d-eb09-4632-a114-93dcb9f1f54a) 
#  and the key of file (LTER_EU_AT_003_ZOEBELBODEN_FOLIAGE_CHEMISTRY_2017_v20201004.zip):
# files": [
#     {
#         "bucket": "ff39d40d-eb09-4632-a114-93dcb9f1f54a",
#         "checksum": "md5:4c3989a182da9cf2a58ebd303eb9c8b2",
#         "ePIC_PID": "https://epic-pid.storage.surfsara.nl:8003/0000/LTER_EU_AT_003_ZOEBELBODEN_FOLIAGE_CHEMISTRY_2017_v20201004.zip",
#         "key": "LTER_EU_AT_003_ZOEBELBODEN_FOLIAGE_CHEMISTRY_2017_v20201004.zip",
#         "size": 6613,
#         "version_id": "1c47a2e2-61e1-458a-8fab-5bebdaa0e334"
#     }
# ],

# 3.2 2nd case: https://deims.org/api/datasets/aacdbaa2-03a4-417f-a415-220ac7a121ff where the title is "B2Share download link"
# ... onlineDistribution": {
#     "dataPolicyUrl": null,
#     "doi": null,
#     "onlineLocation": [
#         {
#             "function": "file_download",
#             "url": {
#                 "title": "B2Share download link",
#                 "value": "https://b2share.eudat.eu/api/files/5f4a92dd-8928-4a13-8be6-28a9303f717e/LAI_LTER_EU_AT_003_zoeb_LAI20182019_v20210119.7z"
#             },
#             "email": null
#         }
#     ]
# }, ...
# 3.2.1 the data can be downloaded directly from b2share, in this case the DOI is not reacheable from B2Share website and can be reached in the the eLTER dataset
#  JSON record if presented or added from the manager after the uploading operation.

# 3.3 3tr case: if the dataset is stored in SOS the value of "onlineDistribution" "function" is "ogc_sos_2_0_0"
# onlineDistribution": {
#     "dataPolicyUrl": null,
#     "doi": null,
#     "onlineLocation": [
#         {
#             "function": "ogc_sos_2_0_0",
#             "url": {
#                 "title": "",
#                 "value": "https://www.sos.it"
#             },
#             "email": null
#         }
#     ]
# },

# 4. probably is possible to download the file directly from file section in the B2Share API of the records (ePIC_PID):
# ... files": [
# 
#     {
#         "bucket": "47efef17-3470-4583-86b7-4b2144c0130c",
#         "checksum": "md5:2af0dc7b2b5c7c8c772bd08e62100224",
#         "ePIC_PID": "http://hdl.handle.net/11304/97b07323-9386-4649-86be-ba28af12e13e",
#         "key": "AT_SI000049_AC_2012.xls",
#         "size": 262656,
#         "version_id": "9c73740e-c1d0-4ea9-b0b1-5f887f78f401"
#     }
# 
# ],
# "id": "2ec9cd568cfd40aa899beadcc8fe1b86", ...

# example about point 3.1. 
resourceID <- 'https://deims.org/dataset/cd1fb6f8-5e57-11e3-aa73-005056ab003f' # LTER Zöbelboden, Austria, Air chemistry, 2012
resourceID <- 'https://deims.org/dataset/79cf34dd-0c01-4d67-809b-67416dfeccb0' # LTER Zöbelboden, Austria, Foliage chemistry, 2019
resourceID <- 'https://deims.org/dataset/38d604ef-decb-4d67-8ac3-cc843d10d3ef' # LTER Northern Adriatic Sea (Italy) marine data from 1965 to 2015

getDataset <- function(resourceID) {
  # connection with DEIMS-SDR related resource
  library(dplyr)
  q = '{title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       online: .attributes.onlineDistribution
      }'
  url <- gsub('/dataset/', "/api/datasets/", resourceID)
  export <- httr::RETRY("GET", url = url, times = 5)
  jj <- suppressMessages(httr::content(export, "text"))
  invisible(capture.output(resource <- tibble::as_tibble(do_Q(q, jj))))
  resourceOnLine <- resource$online.onlineLocation[[1]]
  
  # type of links provided by DEIMS-SDR related resource
  downloadLink <- resourceOnLine %>% dplyr::filter(`function` == 'file_download' & url$title == 'B2Share Download Link') # issue b
  landingPage <- resourceOnLine %>% dplyr::filter(`function` == 'url' & url$title == 'B2Share Landing Page') # issue b
  sosEndpoint <- resourceOnLine %>% dplyr::filter(`function` == 'ogc_sos_2_0_0')
  
  # by use of landingPage link, connection with B2Share
  qLP = '{
     links: .links,
     files: .files
    }'
  landingPageURI <- gsub('/records/', "/api/records/", landingPage$url$value) # issue b
  exportLP <- httr::RETRY("GET", url = landingPageURI, times = 5)
  jjLP <- suppressMessages(httr::content(exportLP, "text"))
  invisible(capture.output(resourceLP <- tibble::as_tibble(do_Q(qLP, jjLP))))
  
  # file information 
  fileLink <- resourceLP$files[[1]]$ePIC_PID[[1]] # issue c
  fileName <- resourceLP$files[[1]]$key[[1]]
  fileExtension <- sub("^[^.]*", "", fileName)
  
  # download of the file
  utils::download.file(fileLink, method = 'wget', destfile = paste0('./', fileName))
  
  
  # based on the file extension (e.g. csv, xls, xslx, zip, etc.)
  if (fileExtension == '.csv') {
    eLTERData <- utils::read.csv2(file = paste0('./', fileName), header = TRUE, sep = ',') %>% as_tibble() # issue f 
  } else if (fileExtension == '.xsl') {
    eLTERData <- gdata::read.xls(xls = paste0('./', fileName), header = TRUE) %>% as_tibble()
  }
}

