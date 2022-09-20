#' Obtain the observed properties measured by a sensor.
#' @description `r lifecycle::badge("experimental")`
#' This function obtains the observed properties by
#' procedure/sensor through Sensor Observation Service (SOS).
#' @param procedure A `character`. It is a procedure/sensor ID.
#' @param sosURL A `character`. The endpoint of the Sensor Observation Service
#' (SOS) service.
#' @return The output of the function is a `tibble` with the labels and URI
#' (Uniform Resource Identifier) of each observed property, the code and
#' URI of Units Of Measurement (UOM) of the observed properties as declared
#' in the Sensor Observation Service (SOS). Codes and URIs as stated in
#' \href{https://qudt.org}{QUDT.org} are also present.
#' QUDT is a public charity nonprofit organization founded to provide
#' semantic specifications for units of measure, quantity kind, dimensions
#' and data types.
#' NB this function returns a valued string only in the case where the
#' UOM refers to a NERC vocabulary term (e.g.
#' http://vocab.nerc.ac.uk/collection/P06/current/UPAA/ for °C).
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @importFrom SPARQL SPARQL
#' @importFrom xml2 xml_attr xml_find_all xml_text
#' @importFrom tibble tibble add_row tibble_row add_column
#' @export
#' @examples
#' FP <- get_sensor_observed_properties(
#'   sosURL = "http://getit.lteritalia.it/observations/service",
#'   procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/1286194C-A5DF-11DF-8ED7-1602DFD72097"
#' )
#' FP
#'
#' eurac_monalisa <- get_sensor_observed_properties(
#'   sosURL = "http://monalisasos.eurac.edu/sos/service",
#'   procedure = "QuantumSensor_nemef2000"
#' )
#' eurac_monalisa
#'
#' obsProsAir <- get_sensor_observed_properties(
#'   sosURL = "http://getit.lteritalia.it/observations/service",
#'   procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/SI000049-1675AirTemp"
#' )
#' obsProsAir
#' 
#' NIVA <- get_sensor_observed_properties(
#'   sosURL = "https://hydro-sos.niwa.co.nz/",
#'   procedure = "Hydrometric_Station"
#' )
#' NIVA
#'
### function get_sos_observed_properties
get_sensor_observed_properties <- function(sosURL, procedure) {
  requestObs <- paste0(
    sosURL,
    "?service=SOS&version=2.0.0&request=DescribeSensor&procedure=",
    procedure,
    "&procedureDescriptionFormat=",
    "http%3A%2F%2Fwww.opengis.net%2Fsensorml%2F2.0"
  )
  # description of sensor ----
  describeSensorXML <- xml2::read_xml(requestObs)
  # Observed Properties ----
  obsProperties <- xml2::xml_attr(
    xml2::xml_find_all(
      describeSensorXML,
      ".//sml:output"
    ),
    "name"
  )
  if (length(obsProperties) != 0) {
    obsPropertiesURI <- xml2::xml_attr(
      xml2::xml_find_all(
        describeSensorXML,
        ".//sml:output/swe:Quantity"
      ),
      "definition"
    )
    obsUOM <- xml2::xml_attr(
      xml2::xml_find_all(
        describeSensorXML,
        ".//sml:output/swe:Quantity/swe:uom"
      ),
      "code"
    )
    obsUomURI <-
      xml2::xml_attr(
        xml2::xml_find_all(
          describeSensorXML,
          ".//sml:output/swe:Quantity/swe:uom"
        ),
        "href"
      )
    if (!is.na(any(match(obsProperties, 'phenomenonTime')))) {
      index <- which(
        match(obsProperties, 'phenomenonTime') == "1"
      )
      obsPropertiesURI <-
        append(
          x = obsPropertiesURI,
          values = 'http://www.opengis.net/def/property/OGC/0/PhenomenonTime',
          after = index - 1
        )
      obsUOM <-
        append(
          x = obsUOM,
          values = '',
          after = index - 1
        )
      obsUomURI <-
        append(
          x = obsUomURI,
          values = 'http://www.opengis.net/def/uom/ISO-8601/0/Gregorian',
          after = index - 1
        )
    } else {
      obsProperties <- obsProperties
    }
    if (length(obsPropertiesURI) == 0) {
      obsPropertiesURI <- rep("", length(obsProperties))
    }
    if (length(obsUOM) == 0) {
      obsUOM <- rep("", length(obsProperties))
    }
    if (length(obsUomURI) == 0) {
      obsUomURI <- rep("", length(obsProperties))
    }
    
    obsProps <-
      tibble::tibble(
        obsProLabel = obsProperties,
        obsProURI = obsPropertiesURI,
        obsProCode = obsUOM,
        obsProUomURI = obsUomURI
      )
    
    # SPARQL for obtain UOM from QUDT
    ireaEndpoint <- "http://fuseki1.get-it.it/directory/query"
    qudtTibble <- tibble::tibble(
      obsProQudtCode = as.character(),
      obsProQudtURI = as.character()
    )
    # only for UOM URI derived from NERC vocabularies
    for (i in 1:nrow(obsProps)) {
      if (grepl("vocab.nerc.ac.uk", obsProps$obsProUomURI[1])) {
        qudtTibble <- qudtTibble %>%
          tibble::add_row(
            tibble::tibble_row(
              obsProQudtCode = "",
              obsProQudtURI = ""
            )
          )
      } else {
        if (obsProps$obsProCode[i] == "") {
          qudtTibble <- qudtTibble %>%
            tibble::add_row(
              tibble::tibble_row(
                obsProQudtCode = "",
                obsProQudtURI = ""
              )
            )
        } else if (obsProps$obsProCode[i] == "ug/l") {
          qudtTibble <- qudtTibble %>%
            tibble::add_row(
              tibble::tibble_row(
                obsProQudtCode = "ug.L-1",
                obsProQudtURI = "<https://qudt.org/vocab/unit/MicroGM-PER-L>"
              )
            )
        } else if (obsProps$obsProCode[i] == "Dmnless") {
          qudtTibble <- qudtTibble %>%
            tibble::add_row(
              tibble::tibble_row(
                obsProQudtCode = "",
                obsProQudtURI = ""
              )
            )
        } else {
          ireaQuery <- paste0(
            "PREFIX owl: <http://www.w3.org/2002/07/owl#>
       PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
       PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
       PREFIX qudt: <http://qudt.org/schema/qudt/>
       SELECT ?c ?l ?code ?s
        WHERE {
          SERVICE <http://vocab.nerc.ac.uk/sparql/sparql> {
            ?c rdf:type skos:Concept .
            <http://vocab.nerc.ac.uk/collection/P06/current/> skos:member ?c .
            OPTIONAL {
              ?c skos:altLabel ?l .
              ?c owl:sameAs ?s .
            }
            FILTER(?l = '",
            obsProps$obsProCode[i],
            # uom,
            "')
            FILTER(REGEX(STR(?s), 'qudt'))
          }
          SERVICE <https://www.qudt.org/fuseki/qudt/query> {
  	        ?s qudt:udunitsCode ?code
          }
        }
      ORDER BY ASC(?l)
      LIMIT 1"
          )
          qudtUOM <- SPARQL::SPARQL(
            url = ireaEndpoint,
            query = ireaQuery,
            curl_args = list(.encoding = "UTF-8")
          )
          qudtTibble[i, ] <- qudtUOM$results[, c(3:4)]
        }
      }
    }
    
    obsProps <- obsProps %>%
      tibble::add_column(
        qudtTibble
      )
    return(obsProps)
  } else {
    message(
      "\n----\n",
      "This sensor does not provides observed properties.\n",
      "----\n"
    )
    obsProps <- NULL
    return(obsProps)
  }
}
