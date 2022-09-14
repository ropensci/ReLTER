#' List the procedures of a Sensor Observations Service (SOS).
#' @description Return a list of procedures (e.g. method, algorithm,
#' instrument, sensor, or system which may be used in making observations)
#' store into a
#' \href{http://opengeospatial.github.io/e-learning/sos/text/index.html}{SOS
#' (Sensor Observations Service OGC)}.
#' @param sosHost A `list`. An SOS endpoint (e.g.
#' \url{http://getit.lteritalia.it/observations/sos/kvp?}).
#' @return The output of the function is a `list` with the name and URI
#' (Uniform Resource Identifier) of each procedure.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @author Paolo Tagliolato, PhD (2021) \email{tagliolato.p@@irea.cnr.it}
#' @importFrom xml2 read_xml xml_ns xml_find_all
#' @importFrom xslt xml_xslt
#' @importFrom utils read.csv
#' @importFrom Rdpack reprompt
#' @references
#'   \insertRef{xml2R}{ReLTER}
#'
#'   \insertRef{xsltR}{ReLTER}
#'
#'   \insertRef{utilsR}{ReLTER}
#' @export
#' @examples
#' \dontrun{
#' get_sos_procedurelist(
#'   sosURL = "http://getit.lteritalia.it/observations/service"
#' )
#' }
#'
### function get_sos_procedurelist
get_sos_procedurelist <- function(sosURL) {
  # FIX this the error is: "Error in open.connection(x, "rb") :
  # SSL: no alternative certificate subject name matches target host name
  # 'www.get-it.it'"
  xslProcUrl.url <- paste0("https://www.get-it.it/objects/sensors/xslt/",
                           "Capabilities_proceduresUrlList.xsl")
  styleProcUrl <- xml2::read_xml(xslProcUrl.url, package = "xslt")
  listProcedure <- utils::read.csv(text = xslt::xml_xslt((
    xml2::read_xml(
      paste0(
        sosURL,
        "?service=SOS&request=",
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
          sosURL,
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
