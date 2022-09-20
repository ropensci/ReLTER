queries_jq <- list(
  "1.0" = list(
    activity_info = list(
      path = "activities",
      query = '{
       title: .title,
       abstract: .attributes.general.abstract,
       keywords: .attributes.general.keywords,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       type: .type,
       created: .created,
       changed: .changed,
       relatedSite: .attributes.general.relatedSite,
       contacts: .attributes.contact,
       boundaries: .attributes.geographic.boundaries,
       observationParameters: .attributes.observations.parameters,
       relatedResources: .attributes.relatedResources
      }'
    ),
    dataset_info = list(
      path = "datasets",
      query = '{
       title: .title,
       abstract: .attributes.general.abstract,
       keywords: .attributes.general.keywords,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       type: .type,
       dateRange: .attributes.general.dateRange,
       relatedSite: .attributes.general.relatedSite,
       contacts: .attributes.contact,
       observationParameters: .attributes.observations.parameters,
       observationSpecies: .attributes.observations.speciesGroups,
       dataPolicy: .attributes.onlineDistribution.dataPolicyUrl,
       doi: .attributes.onlineDistribution.doi,
       onlineLocation: .attributes.onlineDistribution.onlineLocation,
       legal: .attributes.legal,
       method: .attributes.method,
       boundaries: .attributes.geographic[].boundaries,
       boundariesDescription: .attributes.geographic[].abstract
      }'
    ),
    site_affiliations = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       affiliation: .attributes.affiliation
      }'
    ),
    site_boundaries = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       boundaries: .attributes.geographic.boundaries
      }'
    ),
    site_contact = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       generalInfo: .attributes.contact
      }'
    ),
    site_envCharacts = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       envCharacteristics: .attributes.environmentalCharacteristics
      }'
    ),
    site_general = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       generalInfo: .attributes.general
      }'
    ),
    site_info = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation
      }'
    ),
    site_infrastructure = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       generalInfo: .attributes.infrastructure
      }'
    ),
    site_parameters = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       parameter: .attributes.focusDesignScale.parameters
      }'
    ),
    site_relatedResources = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       relatedResources: .attributes.relatedResources
      }'
    ),
    site_researchTopics = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       researchTopics: .attributes.focusDesignScale.researchTopics
      }'
    ),
    sensor_info = list(
      path = "sensors",
      query = '{
        title: .title,
        abstract: .attributes.general.abstract,
        keywords: .attributes.general.keywords,
        uri: "\\(.id.prefix)\\(.id.suffix)",
        type: .type,
        created: .created,
        changed: .changed,
        relatedSite: .attributes.general.relatedSite,
        contacts: .attributes.general.contact,
        geography: .attributes.geographic.coordinates,
        elevation: .attributes.geographic.elevation.value,
        sensorType: .attributes.observation.sensorType,
        resultAcquisitionSource: .attributes.observation.resultAcquisitionSource,
        observedProperty: .attributes.observation.observedProperty
      }'
    )
  ),
  "1.1" = list(
    activity_info = list(
      path = "activities",
      query = '{
       title: .title,
       abstract: .attributes.general.abstract,
       keywords: .attributes.general.keywords,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       type: .type,
       created: .created,
       changed: .changed,
       relatedSite: .attributes.general.relatedSite,
       contacts: .attributes.contact,
       boundaries: .attributes.geographic.boundaries,
       observationParameters: .attributes.observations.parameters,
       relatedResources: .attributes.relatedResources
      }'
    ),
    dataset_info = list(
      path = "datasets",
      query = '{
       title: .title,
       abstract: .attributes.general.abstract,
       keywords: .attributes.general.keywords,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       type: .type,
       dateRange: .attributes.general.dateRange,
       relatedSite: .attributes.general.relatedSite,
       contacts: .attributes.contact,
       observationParameters: .attributes.observations.parameters,
       observationSpecies: .attributes.observations.speciesGroups,
       dataPolicy: .attributes.onlineDistribution.dataPolicyUrl,
       doi: .attributes.onlineDistribution.doi,
       onlineLocation: .attributes.onlineDistribution.onlineLocation,
       legal: .attributes.legal,
       method: .attributes.method,
       boundaries: .attributes.geographic[].boundaries,
       boundariesDescription: .attributes.geographic[].abstract
      }'
    ),
    site_affiliations = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       affiliation: .attributes.affiliation
      }'
    ),
    site_boundaries = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       boundaries: .attributes.geographic.boundaries
      }'
    ),
    site_contact = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       generalInfo: .attributes.contact
      }'
    ),
    site_envCharacts = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       envCharacteristics: .attributes.environmentalCharacteristics
      }'
    ),
    site_general = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       generalInfo: .attributes.general
      }'
    ),
    site_info = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation
      }'
    ),
    site_infrastructure = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       generalInfo: .attributes.infrastructure
      }'
    ),
    site_parameters = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       parameter: .attributes.focusDesignScale.observedProperties
      }'
    ),
    site_relatedResources = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       relatedResources: .attributes.relatedResources
      }'
    ),
    sensor_info = list(
      path = "sensors",
      query = '{
        title: .title,
        abstract: .attributes.general.abstract,
        uri: "\\(.id.prefix)\\(.id.suffix)",
        type: .type,
        created: .created,
        changed: .changed,
        relatedSite: .attributes.general.relatedSite,
        contacts: .attributes.general.contact,
        geography: .attributes.geographic.coordinates,
        elevation: .attributes.geographic.elevation.value,
        sensorType: .attributes.observation.sensorType,
        resultAcquisitionSource: .attributes.observation.resultAcquisitionSource,
        observedProperty: .attributes.observation.observedProperty
      }'
    )
  )
)
