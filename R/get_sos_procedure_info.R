#' Obtain the information from a sensor.
#' @description `r lifecycle::badge("experimental")`
#' This function obtains the information (metadata) shared by
#' procedure/sensor through Sensor Observation Service (SOS).
#' @param sosURL A `character`. The endpoint of the Sensor Observation Service
#' (SOS) service.
#' @param procedure A `character`. It is a procedure/sensor ID.
#' @return The output of the function is a `character` containing attributes
#' such as name of the sensor, id and their description.
#' All the information are collected by requests to Sensor Observation
#' Service (SOS).
#' @author Alessandro Oggioni, phD\email{oggioni.a@@irea.cnr.it}
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @importFrom xml2 xml_text xml_find_all
#' @export
#' @examples
#' FP <- get_sos_procedure_info(
#'   sosURL = "http://getit.lteritalia.it/observations/service",
#'   procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/1286194C-A5DF-11DF-8ED7-1602DFD72097"
#' )
#' FP
#'
#' 
#' # \href{https://www.eurac.edu/}{EURAC} SOS 
#' # eurac_monalisa <- get_sos_procedure_info(
#' #   sosURL = "http://monalisasos.eurac.edu/sos/service",
#' #   procedure = "QuantumSensor_nemef2000"
#' # )
#' # eurac_monalisa
#' 
#' # obsProsAir <- get_sos_procedure_info(
#' #   sosURL = "http://getit.lteritalia.it/observations/service",
#' #   procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/SI000049-1675AirTemp"
#' # )
#' # obsProsAir
#' 
#' 
#' # \href{https://niwa.co.nz}{NIVA}
#' # NIVA <- get_sos_procedure_info(
#' #   sosURL = "https://hydro-sos.niwa.co.nz/",
#' #   procedure = "Water_Quality_Site"
#' # )
#' # NIVA
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
