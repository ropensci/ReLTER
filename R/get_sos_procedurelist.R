#' @title eLTER get_sos_procedurelist function
#' @description This function lists the procedures
#' of a Sensor Observations Service (SOS).
#' @param sosHost a `character`. An SOS endpoint (e.g.
#' \url{http://vesk.ve.ismar.cnr.it/observations/sos/kvp?}).
#' @return The output of the function is a `list` with the name and URI (Uniform
#' Resource Identifier) of each procedure.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom xml2 read_xml xml_ns xml_find_all
#' @importFrom xslt xml_xslt
#' @importFrom utils read.csv
#' @export
#' @examples
#' \dontrun{
#' get_sos_procedurelist(sosHost = "http://getit.lteritalia.it/observations/sos/kvp?")
#' }
#'
### function get_sos_procedurelist
get_sos_procedurelist <- function(sosHost) {
  xslProcUrl.url <- paste0("https://www.get-it.it/objects/sensors/xslt/",
                           "Capabilities_proceduresUrlList.xsl")
  styleProcUrl <- xml2::read_xml(xslProcUrl.url, package = "xslt")

  listProcedure <- utils::read.csv(text = xslt::xml_xslt((
    xml2::read_xml(
      paste0(
        sosHost,
        "service=SOS&request=",
        "GetCapabilities&Sections=Contents"
      ),
      package = "xslt"
    )
  ), styleProcUrl), header = TRUE, sep = ";")

  sensorName <- vector(mode = "character", length = nrow(listProcedure))
  for (i in seq_len(nrow(listProcedure))) {
    SensorML <- xml2::read_xml(
      as.character(
        paste0(
          sosHost,
          "service=SOS&amp;version=2.0.0&amp;",
          "request=DescribeSensor&amp;procedure=",
          listProcedure$uri[i],
          "&amp;procedureDescriptionFormat=http%3A%2F%2Fwww.opengis.net",
          "%2FsensorML%2F1.0.1"
        )
      )
    )
    ns <- xml2::xml_ns(SensorML)
    sensorName[i] <- as.character(
      xml2::xml_find_all(
        SensorML,
        paste0("//sml:identification/sml:IdentifierList/sml:identifier",
               "[@name='shortName']/sml:Term/sml:value/text()"),
        ns
      )
    )
  }
  listProcedure <- as.list(listProcedure$uri)
  names(listProcedure) <- sensorName
  return(listProcedure)
}
