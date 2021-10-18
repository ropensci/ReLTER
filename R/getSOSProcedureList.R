#' @title eLTER getSOSProcedureList function
#' @description This function list the procedures of a Sensor
#' Observations Service (SOS).
#' @param sosHost a `character`. It is a SOS endpoint (e.g.
#' http://getit.lteritalia.it). In particular is the path before
#' '/observations/service?'
#' @return The output of the function is a `list` with the name and URI (Uniform
#' Resource Identifier) of each procedure.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom xml2 read_xml xml_ns xml_find_all
#' @export
#' @examples
#' getSOSProcedureList(sosHost = "http://getit.lteritalia.it")
#'
### function getProcedureList
# TODO: testare questo quando il GET-IT è attivo
getSOSProcedureList <- function(sosHost) {
  xslProcUrl.url <- "https://www.get-it.it/objects/sensors/xslt/Capabilities_proceduresUrlList.xsl"
  styleProcUrl <- xml2::read_xml(xslProcUrl.url, package = "xslt")

  listProcedure <- read.csv(text = xslt::xml_xslt((
    xml2::read_xml(
      paste0(
        sosHost,
        "/observations/service?service=SOS&request=GetCapabilities&Sections=Contents"
      ),
      package = "xslt"
    )
  ), styleProcUrl), header = TRUE, sep = ";")

  sensorName <- vector(mode = "character", length = nrow(listProcedure))
  for (i in 1:nrow(listProcedure)) {
    SensorML <- xml2::read_xml(
      as.character(
        paste0(
          sosHost,
          "/observations/service?service=SOS&amp;version=2.0.0&amp;request=DescribeSensor&amp;procedure=",
          listProcedure$uri[i],
          "&amp;procedureDescriptionFormat=http%3A%2F%2Fwww.opengis.net%2FsensorML%2F1.0.1"
        )
      )
    )
    ns <- xml2::xml_ns(SensorML)
    sensorName[i] <- as.character(
      xml2::xml_find_all(
        SensorML,
        "//sml:identification/sml:IdentifierList/sml:identifier[@name='short name']/sml:Term/sml:value/text()",
        ns
      )
    )
  }
  listProcedure <- as.list(listProcedure$uri)
  names(listProcedure) <- sensorName
  return(listProcedure)
}
