#' @examples
#' FP <- get_sos_procedure_info(
#'   sosURL = "http://getit.lteritalia.it/observations/service",
#'   procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/1286194C-A5DF-11DF-8ED7-1602DFD72097"
#' )
#' FP
#'
#' eurac_monalisa <- get_sos_procedure_info(
#'   sosURL = "http://monalisasos.eurac.edu/sos/service",
#'   procedure = "QuantumSensor_nemef2000"
#' )
#' eurac_monalisa
#' 
#' # umweltbundesamt <- get_sos_procedure_info(
#' #   sosURL = "https://ltercwn.umweltbundesamt.at/cwn-sos2/service",
#' #   procedure = "KAS_M"
#' # )
#' # The value 'D:/deployment/projects/cwnsos2node0140100/Tomcat/sensorML/sos2/sml2/KAS_M.xml' of the parameter 'procedure' is invalid
#'
#'
#' obsProsAir <- get_sos_procedure_info(
#'   sosURL = "http://getit.lteritalia.it/observations/service",
#'   procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/SI000049-1675AirTemp"
#' )
#' obsProsAir
#' 
#' NIVA <- get_sos_procedure_info(
#'   sosURL = "https://hydro-sos.niwa.co.nz/",
#'   procedure = "Hydrometric_Station"
#' )
#' NIVA
#'
### function get_sos_procedure_info
get_sos_procedure_info <- function(sosURL, procedure) {
  requestProc <- paste0(
    sosURL,
    "?service=SOS&version=2.0.0&request=DescribeSensor&procedure=",
    procedure,
    "&procedureDescriptionFormat=",
    "http%3A%2F%2Fwww.opengis.net%2Fsensorml%2F2.0"
  )
  # description of sensor ----
  describeSensorXML <- xml2::read_xml(requestProc)
  proc <- xml2::xml_text(
    xml2::xml_find_all(
      describeSensorXML,
      ".//gml:name"
    )
  )
  if (length(proc) == 0) {
    proc <- xml2::xml_text(
      xml2::xml_find_all(
        describeSensorXML,
        ".//gml:identifier"
      )
    )
  }
  attr(
    x = proc,
    which = "id"
  ) <- procedure
  attr(
    x = proc,
    which = "description"
  ) <- xml2::xml_text(
    xml2::xml_find_all(
      describeSensorXML,
      ".//gml:description"
    )
  )
  return(proc)
}
