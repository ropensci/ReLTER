#' @title eLTER_getNetworkParameters
#' @description This function ...
#' @param ...
#' @return The output of the function is ...
#' @author Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>
#' @import jsonlite
#' @export
#' @examples
#' getSOSProcedureList(sosHost = 'https://...')
#'
### function getProcedureList
getSOSProcedureList <- function(sosHost) {
  # xslProcUrl.url <- "https://www.get-it.it/objects/sensors/xslt/Capabilities_proceduresUrlList.xsl"
  xslProcUrl.url <- "./xslt/Capabilities_proceduresUrlList.xsl"
  styleProcUrl <- read_xml(xslProcUrl.url, package = "xslt")

  listProcedure <- read.csv(text = xml_xslt((
    read_xml(
      paste0(sosHost,
             '/observations/service?service=SOS&request=GetCapabilities&Sections=Contents'),
      package = "xslt"
    )
  ), styleProcUrl), header = TRUE, sep = ';')

  sensorName <- vector("character", nrow(listProcedure))
  for (i in 1:nrow(listProcedure)) {
    SensorML <- read_xml(
      as.character(
        paste0(
          sosHost,
          '/observations/service?service=SOS&amp;version=2.0.0&amp;request=DescribeSensor&amp;procedure=',
          listProcedure$uri[i],
          '&amp;procedureDescriptionFormat=http%3A%2F%2Fwww.opengis.net%2FsensorML%2F1.0.1'
        )
      )
    )
    ns <- xml_ns(SensorML)
    sensorName[i] <- as.character(xml_find_all(
      SensorML,
      "//sml:identification/sml:IdentifierList/sml:identifier[@name='short name']/sml:Term/sml:value/text()",
      ns
    ))

  }
  listProcedure <- as.list(listProcedure$uri)
  names(listProcedure) <- sensorName
  return(listProcedure)
}
