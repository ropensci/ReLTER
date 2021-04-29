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
  export <- httr::GET(url = url)
  jj <- suppressMessages(httr::content(export, "text"))
  invisible(capture.output(resource <- tibble::as_tibble(ReLTER::do_Q(q, jj))))
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
  exportLP <- httr::GET(url = landingPageURI)
  jjLP <- suppressMessages(httr::content(exportLP, "text"))
  invisible(capture.output(resourceLP <- tibble::as_tibble(ReLTER::do_Q(qLP, jjLP))))
  
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

