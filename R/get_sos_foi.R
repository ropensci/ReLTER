#' @examples
#' FP <- get_sos_foi(
#'   sosURL = "http://getit.lteritalia.it/observations/service",
#'   procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/1286194C-A5DF-11DF-8ED7-1602DFD72097"
#' )
#' FP
#'
#' eurac_monalisa <- get_sos_foi(
#'   sosURL = "http://monalisasos.eurac.edu/sos/service",
#'   procedure = "QuantumSensor_nemef2000"
#' )
#' eurac_monalisa
#' 
#' # umweltbundesamt <- get_sos_foi(
#' #   sosURL = "https://ltercwn.umweltbundesamt.at/cwn-sos2/service",
#' #   procedure = "KAS_M"
#' # )
#' # The value 'D:/deployment/projects/cwnsos2node0140100/Tomcat/sensorML/sos2/sml2/KAS_M.xml' of the parameter 'procedure' is invalid
#'
#'
#' obsProsAir <- get_sos_foi(
#'   sosURL = "http://getit.lteritalia.it/observations/service",
#'   # procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/SI000049-1675AirTemp",
#'   foiURI = "http://www.get-it.it/featureOfInterest/SI000049-1675"
#' )
#' obsProsAir
#' 
#' NIVA <- get_sos_foi(
#'   sosURL = "https://hydro-sos.niwa.co.nz/",
#'   procedure = "Hydrometric_Station"
#' )
#' NIVA
#'
### function get_sos_foi
get_sos_foi <- function(sosURL, procedure = NULL, foiURI = NULL) {
  requestFOI <- list()
  if (!is.null(procedure) & is.null(foiURI)) {
    requestObs <- paste0(
      sosURL,
      "?service=SOS&version=2.0.0&request=GetObservation",
      "&procedure=",
      procedure
    )
    observationDataXML <- xml2::read_xml(requestObs)
    foiLabel <- xml2::xml_attr(xml2::xml_find_all(observationDataXML, ".//om:featureOfInterest"), "title")
    foiURI <- xml2::xml_attr(xml2::xml_find_all(observationDataXML, ".//om:featureOfInterest"), "href")
    for (i in 1:length(foiURI)) {
      requestFOI[[i]] <- paste0(
        sosURL,
        "?service=SOS&version=2.0.0&request=GetFeatureOfInterest&featureOfInterest=",
        foiURI[[i]]
      )
    }
  } else {
    foiURI = foiURI
    for (i in 1:length(foiURI)) {
      requestFOI[[i]] <- paste0(
        sosURL,
        "?service=SOS&version=2.0.0&request=GetFeatureOfInterest&featureOfInterest=",
        foiURI
      )
    }
  }
  # description of FOI ----
  foiXML <- list()
  foi <- list()
  l=1
  for (l in 1:length(requestFOI)) {
    foiXML[[l]] <- xml2::read_xml(requestFOI[[1]][[l]])
    foi[[l]]["typeSf"] <- xml2::xml_attr(xml2::xml_find_all(foiXML[[l]], ".//sf:type"), "href")
    foi[[l]]["typeSam"] <- xml2::xml_attr(xml2::xml_find_all(foiXML[[l]], ".//sam:type"), "href")
    foi[[l]]["description"] <-  xml2::xml_text(xml2::xml_find_all(foiXML[[l]], ".//gml:description"))
    foi[[l]]["id"] <-  xml2::xml_text(xml2::xml_find_all(foiXML[[l]], ".//gml:id"))
    foi[[l]]["sampledFeature"] <- xml2::xml_attr(xml2::xml_find_all(foiXML[[l]], ".//sf:sampledFeature"), "href")
    foi[[l]]["srsName"] <- xml2::xml_attr(xml2::xml_find_all(foiXML[[l]], ".//gml:pos"), "srsName")
    foi[[l]]["pos"] <- xml2::xml_text(xml2::xml_find_all(foiXML[[l]], ".//gml:pos/text()"))
  }
  
  
  foi <- foiLabel
  attr(
    x = foi,
    which = "type"
  ) <- foiType
  attr(
    x = foi,
    which = "sf"
  ) <- sampledFeature
  attr(
      x = foi,
      which = "srs"
    ) <- srs
  attr(
    x = foi,
    which = "coords"
  ) <- coords
  return(foi)
}
