#' Obtain the observations from a Sensor Observation Service (SOS).
#' @description `r lifecycle::badge("experimental")`
#' This function obtains the observations shared by Sensor Observation
#' Service (SOS).
#' @param sosURL A `character`. The endpoint of the Sensor Observation Service
#' (SOS) service.
#' @param procedure A `character`. The procedure/sensor ID.
#' Possible value are the Feature of Interest(FOI) ID,
#' which can be obtained via the `get_sos_procedure_info()` function.
#' @param foi A `character`. This parameter selects which Feature(s) Of
#' Interest (FOI(s)) are retrieved and returned in the observations.
#' Possible value are the FOI(s) ID, which can be obtained via the
#' `get_sos_foi()` function.
#' If the FOI parameter is not set, all observations reached in all FOIs
#' by a sensor, will be downloaded. Note that the request time may
#' be very long!
#' Multiple values can be indicated.
#' Default NA. 
#' @param show_map A `logical`. When TRUE the boundary will be plotted on a
#' Leaflet map. Default FALSE.
#' @return The output of the function is a `tibble`. The output
#' can be divided in two parts: the columns concerning the observations and the
#' ancillary information concerning who and where the observations were made.
#' The first part, usually starting with date and time values, contains all
#' columns representing all the observed properties (e.g. air temperature)
#' measured by sensor.
#' The second part contains columns about: Feature(s) Of Interest - FOI (
#' foiLabel and foiID), identifier of the observations block (obsBlockID), 
#' procedure/sensor (procedureID and procedureName), sampling feature (typeSf,
#' description, name), sampled feature id (sampledFeature), coordinate reference
#' system code (srsName) and coordinates (lon and lat).
#' A map can be obtained indicating the parameter `show_map` TRUE.
#' The output contains also a semantic link, as provided by SOS. The `uri`
#' attribute contains all URIs of the terms indicated in the headers columns.
#' To the observed properties columns are labeled with a unit of measurement,
#' as mentioned in the SOS, using R package
#' \href{https://r-quantities.github.io/units/index.html}{`units`}.
#' This labelling simplify the propagation,
#' conversion and derivation of units of collected observed properties.
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @importFrom xml2 read_xml xml_attr xml_find_all xml_text xml_find_first
#' @importFrom tidyr separate_rows separate
#' @importFrom dplyr mutate mutate_if arrange filter full_join select
#' @importFrom dplyr group_by
#' @importFrom lubridate ymd_hms
#' @importFrom tibble tibble add_row tibble_row as_tibble
#' @importFrom httr2 request req_url_query req_method req_headers
#' @importFrom httr2 req_user_agent req_retry req_perform resp_check_status
#' @importFrom httr2 resp_body_json
#' @importFrom sf st_as_sf
#' @importFrom leaflet leaflet addTiles addMarkers
#' @importFrom units set_units
#' @importFrom purrr map_dfr
#' @import RCurl
#' @import XML
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#' 
#' # Fluoro Probe sensor
#' # FP <- get_sos_obs(
#' #   sosURL = "http://getit.lteritalia.it/observations/service",
#' #   procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/1286194C-A5DF-11DF-8ED7-1602DFD72097",
#' #   foi = c("http://www.get-it.it/sensors/getit.lteritalia.it/sensors/foi/SSF/SP/4326/45.9547/8.63403"),
#' #   show_map = TRUE
#' # )
#' # FP
#' 
#' # Air temperature sensor
#' airTemp <- get_sos_obs(
#'   sosURL = "http://getit.lteritalia.it/observations/service",
#'   procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/SI001469-SPNAirTemp", #720 Slowinski National Park
#'   show_map = TRUE
#' )
#' airTemp
#'
#' # about units of measurement (UOM)
#' # the UOM of this observed property is °C
#' airTemp$Air_Temperature
#'
#' # is is easily convert to °F
#' units::set_units(airTemp$Air_Temperature, "°F")
#'
#' # about semantic enrichment
#' # the URI of the label of first two columns
#' # of `airTemp`
#' attributes(airTemp)$uri
#'   
#' }
#' ## End (Not run)
#' 
#' @section The function output:
#' \figure{get_sos_obs_fig.png}{Map of the point where the observations
#' are collected.}
#' 
#' @section Possible output from function result:
#' \figure{get_sos_obs_plot_C.png}{Plot of the observations where the
#' unit of menasure is expressed in °C}
#' 
#' \figure{get_sos_obs_plot_F.png}{Plot of the same observations are
#' converted in °F}
#' 
### function get_sos_obs
get_sos_obs <- function(sosURL, procedure, foi = NULL, show_map = FALSE) {
  if (is.null(foi)) {
    requestObs <- paste0(
      sosURL,
      "?service=SOS&version=2.0.0&request=GetObservation",
      "&procedure=",
      procedure
    )
  } else {
    foiS <- paste(foi, collapse = ',')
    requestObs <- paste0(
      sosURL,
      "?service=SOS&version=2.0.0&request=GetObservation",
      "&procedure=",
      procedure,
      "&featureOfInterest=",
      foiS
    )
  }
  observationDataXML <- xml2::read_xml(requestObs)
  obsBlockID <- xml2::xml_attr(xml2::xml_find_all(observationDataXML, ".//om:OM_Observation"), "id")
  obsPropURI <- xml2::xml_attr(xml2::xml_find_all(observationDataXML, ".//om:observedProperty"), "href")
  obsPropLabel <- xml2::xml_attr(xml2::xml_find_all(observationDataXML, ".//om:observedProperty"), "title")
  foiLabel <- xml2::xml_attr(xml2::xml_find_all(observationDataXML, ".//om:featureOfInterest"), "title")
  foiID <- xml2::xml_attr(xml2::xml_find_all(observationDataXML, ".//om:featureOfInterest"), "href")
  numValues <- xml2::xml_find_all(observationDataXML, ".//swe:Count/swe:value/text()")
  fieldLabel <- xml2::xml_attr(xml2::xml_find_all(observationDataXML, ".//swe:field"), "name") %>%
    unique()
  nFieldLabel <- length(fieldLabel)
  # NB now is considered only fields with Quantity
  fieldURI <- xml2::xml_attr(xml2::xml_find_all(observationDataXML, ".//swe:field/swe:Quantity"), "definition") %>%
    unique()
  if (any(fieldLabel == "phenomenonTime")) {
    fieldURI <- append(fieldURI, "http://www.opengis.net/def/property/OGC/0/PhenomenonTime", 0)
  }
  fieldUOMLabel <- xml2::xml_attr(xml2::xml_find_all(observationDataXML, ".//swe:uom"), "code") %>%
    .[c(1:nFieldLabel)]
  fieldUOMURI <- xml2::xml_attr(xml2::xml_find_all(observationDataXML, ".//swe:uom"), "href") %>%
    .[c(1:nFieldLabel)]
  encodingDec <- xml2::xml_attr(xml2::xml_find_all(observationDataXML, ".//swe:TextEncoding"), "decimalSeparator")
  encodingToken <- xml2::xml_attr(xml2::xml_find_first(observationDataXML, ".//swe:TextEncoding"), "tokenSeparator")
  encodingBlock <- xml2::xml_attr(xml2::xml_find_first(observationDataXML, ".//swe:TextEncoding"), "blockSeparator")
  valuesAll <- xml2::xml_text(xml2::xml_find_all(observationDataXML, ".//swe:values/text()"))
  beginPosition <- xml2::xml_text(xml2::xml_find_all(observationDataXML, ".//gml:beginPosition/text()"))
  endPosition <- xml2::xml_text(xml2::xml_find_all(observationDataXML, ".//gml:endPosition/text()"))
  # Results ---
  results <- list()
  # NB the quantities like textual is not considered yet
  for (m in 1:length(valuesAll)) {
    results[[m]] <- sub(paste0(encodingBlock, "+$"), "", valuesAll[[m]]) %>%
      tibble::as_tibble() %>%
      tidyr::separate_rows(value, sep = encodingBlock) %>%
      tidyr::separate(value, into = fieldLabel, sep = encodingToken)
    if (any(names(results[[m]]) == "phenomenonTime")) {
      indexTime <- which(names(results[[m]]) == "phenomenonTime")
      results[[m]] <- results[[m]] %>%
        dplyr::mutate(phenomenonTime = lubridate::ymd_hms(phenomenonTime)) %>%
        dplyr::mutate_if(is.character, as.numeric)
    }
    results[[m]]["foiLabel"] <- foiLabel[[m]]
    results[[m]]["foiID"] <- foiID[[m]]
    results[[m]]["obsBlockID"] <- obsBlockID[[m]]
    # Definition semantic enrichment ----
    attr(
      x = results[[m]],
      which = "uri"
    ) <- fieldURI
    # SPARQL for obtain UOM from QUDT
    ireaEndpoint <- "http://fuseki1.get-it.it/directory/query"
    obsProps <- tibble::tibble(
      obsProLabel = fieldLabel,
      obsProURI = fieldURI,
      obsProCode = fieldUOMLabel,
      obsProUomURI = fieldUOMURI
    )
    qudtTibble <- tibble::tibble(
      obsProQudtCode = as.character(),
      obsProQudtURI = as.character()
    )
    # only for UOM URI derived from NERC vocabularies
    for (i in 1:length(fieldUOMLabel)) {
      if (grepl("vocab.nerc.ac.uk", obsProps$obsProUomURI[1])) {
        qudtTibble <- qudtTibble %>%
          tibble::add_row(
            tibble::tibble_row(
              obsProQudtCode = "",
              obsProQudtURI = ""
            )
          )
      } else {
        if (is.na(obsProps$obsProCode[i])) {
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
          qudtUOM <- httr2::request(ireaEndpoint) %>%
            httr2::req_url_query(query = ireaQuery) %>%
            httr2::req_method("POST") %>%
            httr2::req_headers(Accept = "application/sparql-results+json") %>%
            httr2::req_user_agent("ReLTER dev") %>%
            httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
            httr2::req_perform()
          httr2::resp_check_status(qudtUOM)
          qudtUOM_JSON <- httr2::resp_body_json(qudtUOM)
          qudtTibble[i, 1] <- qudtUOM_JSON$results$bindings[[1]]$code$value
          qudtTibble[i, 2] <- qudtUOM_JSON$results$bindings[[1]]$s$value
        }
      }
    }
    obsProps <- obsProps %>%
      tibble::add_column(
        qudtTibble
      )
    for (i in 1:nrow(obsProps)) {
      if (obsProps$obsProQudtCode[i] == "") {
        next
      } else {
        attrName <- obsProps$obsProLabel[i]
        results[[m]] <- results[[m]] %>%
          dplyr::mutate(
            !!attrName := units::set_units(
              eval(parse(text = attrName)),
              obsProps$obsProQudtCode[i],
              mode = "standard"
            )
          )
      }
    }
    if (!is.null(results[[m]])) {
      if (any(names(results[[m]]) == "phenomenonTime")) {
        results[[m]] <- results[[m]] %>%
          dplyr::arrange(phenomenonTime)
      } else {
        results <- NA
        message(
          "\n----\n",
          "This sensor does not have collectes or provides observations yet.\n",
          "----\n"
        )
      }
    }
  }
  results <- purrr::map_dfr(
    .x = results, .f = ~.x
  )
  
  # FOI info ----
  fois_info_service <- get_sos_foi(
    sosURL = sosURL,
    show_map = FALSE
  )
  fois_info_procedure <- results$foiID %>%
    unique()
  fois_selected <- fois_info_service %>%
    dplyr::filter(foiID %in% fois_info_procedure)
  # procedure info ----
  proc <- get_sos_procedure_info(
    sosURL = sosURL,
    procedure = procedure
  )
  # enrich results
  results$procedureID <- attributes(proc)$id
  results$procedureName <- proc[1]
  results <- dplyr::full_join(
    x = results,
    y = fois_selected
  )
  # info geo offering ----
  if (is.null(results)) {
    if (show_map == TRUE) {
      message(
        "\n----\n",
        "This sensor does not have collectes or provides observations yet.\n",
        "The map relating to the distribution of observations cannot",
        "be drawn.\n",
        "----\n"
      )
    }
  } else {
    if (show_map == TRUE) {
      crs <- results$srsName[min(which(results$srsName != ""))]
      # geo of observations ----
      resultsGeo <- results %>%
        dplyr::select(
          foiLabel, foiID,
          #obsBlockID,
          procedureID, procedureName, typeSf,
          description, name, sampledFeature,
          # srsName,
          lon, lat
        ) %>%
        dplyr::group_by(foiID) %>%
        dplyr::mutate(n = dplyr::n()) %>%
        unique() %>%
        sf::st_as_sf(
          coords = c("lat", "lon"),
          crs = crs
        )
      # map ----
      map <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addMarkers(
          data = resultsGeo,
          popup = paste0(
            "<b>Sensor name: </b>",
            "<br>",
            "<a href='",
            resultsGeo$procedureID,
            "' target='_blank'>",
            resultsGeo$procedureName,
            "</a>",
            "<br>",
            "<b>Sensor coordinates: </b>",
            "<br>",
            resultsGeo$geometry,
            "<br>",
            "<b>Observations collected by the sensor in this feature: </b>",
            resultsGeo$n
          )
        )
      print(map)
      # transformation of results to nested tibble entails
      # that the attributes (uri) are lost
      # results <- results %>%
      #   dplyr::group_by(
      #     foiLabel, obsBlockID
      #   ) %>%
      #   tidyr::nest()
      return(results)
    } else {
      # transformation of results to nested tibble entails
      # that the attributes (uri) are lost
      # results <- results %>%
      #   dplyr::group_by(
      #     foiLabel, obsBlockID
      #   ) %>%
      #   tidyr::nest()
      return(results)
    }
  }
}
