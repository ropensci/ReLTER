#' @description This function ...
#' @param procedure a `character`. It is a SOS endpoint of procedure/sensor (URI).
#' @return The output of the function is a `tibble` with the label of observed property and URI (Uniform Resource Identifier) measured by procedure/sensor.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import xml2 R.utils dplyr
#' @export
#' @examples
#' SOSObsProp(procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/1286194C-A5DF-11DF-8ED7-1602DFD72090")
#'
### function SOSObsProp
SOSObsProp <- function(procedure) {
  xmlProcedure <- xml2::read_xml(procedure)
  obsProperties <- xml2::xml_attr(xml2::xml_find_all(xmlProcedure, ".//sml:output"), "name")
  obsPropertiesURI <- xml2::xml_attr(xml2::xml_find_all(xmlProcedure, ".//sml:output/swe:Quantity"), "definition")
  if (any(match(obsProperties, 'phenomenonTime')) == 1) {
    indice <- which(match(obsProperties, 'phenomenonTime') == "1")
    obsPropertiesURI <- R.utils::insert(obsPropertiesURI, ats = indice, values = 'http://www.opengis.net/def/property/OGC/0/PhenomenonTime')
  }
  obsProps <- dplyr::tibble(obsProLabel = obsProperties, obsProURI = obsPropertiesURI)
  obsProps
}

# https://lter-at-sos.sos.cdn.lter-europe.net/service

#' @description This function ...
#' @param procedure a `character`. It is a SOS of procedure/sensor (URI).
#' @return The output of the function is a `tibble` with the label of observed property and URI (Uniform Resource Identifier) measured by procedure/sensor.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import xml2 R.utils dplyr
#' @export
#' @examples
#' SOSObsProp(procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/1286194C-A5DF-11DF-8ED7-1602DFD72090")
#'
### function SOSObsProp of provile observation
obsOfProfile <- function(sosEndpoint, observedProperty) {
  procedure <- xml2::read_xml("http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/1286194C-A5DF-11DF-8ED7-1602DFD72095")
  ObsProperties <- xml2::xml_attr(xml2::xml_find_all(procedure, ".//sml:output"), "name")
  ObsProperties[8] <- 'trasmission'
  ObsProperties[9] <- 'depth'
  
  lterItalyGetObs <- xml2::read_xml("http://getit.lteritalia.it/observations/service?service=SOS&version=2.0.0&request=GetObservation&observedProperty=http://vocabs.lter-europe.net/EnvThes/20166")
  obsProp <- xml2::xml_attr(xml2::xml_find_first(lterItalyGetObs, ".//om:observedProperty"), "href")
  obsPropLab <- xml2::xml_attr(xml2::xml_find_first(lterItalyGetObs, ".//om:observedProperty"), "title")
  uomFirst <- xml2::xml_attr(xml2::xml_find_first(lterItalyGetObs, ".//swe:Quantity/swe:uom"), "code")
  valuesAll <- xml2::xml_find_all(lterItalyGetObs, ".//swe:values/text()")
  numValues <- xml2::xml_find_all(lterItalyGetObs, ".//swe:Count/swe:value/text()")
  foiLabels <- xml2::xml_attr(xml2::xml_find_all(lterItalyGetObs, ".//om:featureOfInterest"), "title")
  # TODO need to add this variable but the value in xml responce don't change with the value of the request inter observations
  # e.g.<swe:encoding>
  # <swe:TextEncoding tokenSeparator="#" blockSeparator="@"/>
  #   </swe:encoding>
  #   <swe:values>
  #   2006-06-22T10:00:30+02;1.74;0.00;1.74;0.00;0.00;20.57;0.00;0.25;77.63##20
  # tokenSeparator <- xml2::xml_attr(xml2::xml_find_all(lterItalyGetObs, ".//swe:TextEncoding"), "tokenSeparator")
  # blockSeparator <- xml2::xml_attr(xml2::xml_find_all(lterItalyGetObs, ".//swe:TextEncoding"), "blockSeparator")
  
  calate <- as_tibble(sapply(ObsProperties, function(x) character())) %>% 
    dplyr::mutate(phenomenonTime = lubridate::ymd_hms(phenomenonTime)) %>% 
    dplyr::mutate(foiLabels = as.character())
  calate[2:10] <- lapply(calate[2:10], function(x) as.numeric())
  
  for (l in 1:length(valuesAll)) {
    calata <- data.frame(x = unlist(strsplit(as.character(valuesAll[l]), "##"))) %>% 
      tidyr::separate(x, ObsProperties, sep = ";", convert = TRUE) %>% 
      as_tibble() %>% 
      dplyr::mutate(foiLabels = foiLabels[l]) %>% 
      dplyr::mutate(phenomenonTime = lubridate::ymd_hms(phenomenonTime))
    calate <- calate %>% tibble::add_row(calata)
  }
  
  calate <- merge(calate, sites, by.x="foiLabels", by.y="names") %>% 
    as_tibble()
  saveRDS(calate, "calate.Rds")
  
  calate <- readRDS("calate.Rds")
  calate <- calate %>% 
    dplyr::mutate_at(dplyr::vars(phenomenonTime), dplyr::funs("date" = lubridate::date(.), "timeUTC" = hms::as_hms(.))) %>% 
    dplyr::select(-LatLong, -coords.x1, -coords.x2) %>% 
    dplyr::mutate(factor = paste0(foiLabels, "-", date))
  calate$factor <- as.factor(calate$factor)
  calate$foiLabels <- as.factor(calate$foiLabels)
  
  if (any(match(obsProperties, 'phenomenonTime')) == 1) {
    
  }
}