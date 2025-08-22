# JQ queries for DEIMS-SDR ----
queries_jq_deims <- list(
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
    site_observedProperties = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       observedProperties: .attributes.focusDesignScale.parameters
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
       abstract: .attributes.general.abstract,
       keywords: .attributes.general.keywords,
       dateRange: .attributes.general.dateRange,
       contacts: .attributes.contact,
       boundaries: .attributes.geographic.boundaries,
       availability: .attributes.availability,
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
       created: .created,
       changed: .changed,
       dateRange: .attributes.general.dateRange,
       relatedSite: .attributes.general.relatedSite,
       contacts: .attributes.contact,
       observationParameters: .attributes.observations.parameters,
       observationSpecies: .attributes.observations.speciesGroups,
       dataPolicy: .attributes.onlineDistribution.dataPolicyUrl,
       doi: .attributes.onlineDistribution.doi,
       onlineDistribution: .attributes.onlineDistribution,
       legal: .attributes.legal,
       method: .attributes.method,
       boundaries: .attributes.geographic[].boundaries,
       boundariesDescription: .attributes.geographic[].abstract,
       elevation: .attributes.geographic[].elevation
      }'
    ),
    location_info_type = list(
      path = "locations",
      query = '{
        geometryType: .geometry.type,
        locationType: .properties.locationType
      }'
    ),
    location_info_point = list(
      path = "locations",
      query = '{
        title: .properties.title,
        abstract: .properties.abstract,
        uri: "\\(.properties.id.prefix)\\(.properties.id.suffix)",
        locationType: .properties.locationType,
        type: .type,
        created: .properties.created,
        changed: .properties.changed,
        relatedSite: .properties.relatedSite,
        geometryType: .geometry.type,
        coordinates: .geometry.coordinates,
        elevation: .properties.elevation,
        images: .properties.images
      }'
    ),
    location_info_polygon = list(
      path = "locations",
      query = '{
        title: .properties.title,
        abstract: .properties.abstract,
        uri: "\\(.properties.id.prefix)\\(.properties.id.suffix)",
        locationType: .properties.locationType,
        type: .type,
        created: .properties.created,
        changed: .properties.changed,
        relatedSite: .properties.relatedSite,
        geometryType: .geometry.type,
        coordinates: .geometry.coordinates[],
        elevation: .properties.elevation,
        images: .properties.images
      }'
    ),
    location_info_multiPolygon = list(
      path = "locations",
      query = '{
        title: .properties.title,
        abstract: .properties.abstract,
        uri: "\\(.properties.id.prefix)\\(.properties.id.suffix)",
        locationType: .properties.locationType,
        type: .type,
        created: .properties.created,
        changed: .properties.changed,
        relatedSite: .properties.relatedSite,
        geometryType: .geometry.type,
        coordinates: .geometry.coordinates[][],
        elevation: .properties.elevation,
        images: .properties.images
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
       # affiliation: .attributes.affiliation,
       networks: .attributes.affiliation.networks,
       projects: .attributes.affiliation.projects
      }'
    ),
    site_boundaries = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       boundaries: .attributes.geographic.boundaries,
       relatedLocations: .attributes.geographic.relatedLocations
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
       # generalInfo: .attributes.contact,
       siteManager: .attributes.contact.siteManager,
       operatingOrganisation: .attributes.contact.operatingOrganisation,
       metadataProvider: .attributes.contact.metadataProvider,
       fundingAgency: .attributes.contact.fundingAgency,
       siteUrl: .attributes.contact.siteUrl
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
       airTemperature: .attributes.environmentalCharacteristics.airTemperature,
       precipitation: .attributes.environmentalCharacteristics.precipitation,
       biogeographicalRegion: .attributes.environmentalCharacteristics.biogeographicalRegion,
       biome: .attributes.environmentalCharacteristics.biome,
       ecosystemType: .attributes.environmentalCharacteristics.ecosystemType,
       eunisHabitat: .attributes.environmentalCharacteristics.eunisHabitat,
       landforms: .attributes.environmentalCharacteristics.landforms,
       geoBonBiome: .attributes.environmentalCharacteristics.geoBonBiome,
       geology: .attributes.environmentalCharacteristics.geology,
       hydrology: .attributes.environmentalCharacteristics.hydrology,
       soils: .attributes.environmentalCharacteristics.soils,
       vegetation: .attributes.environmentalCharacteristics.vegetation
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
       abstract: .attributes.general.abstract,
       status: .attributes.general.status,
       yearEstablished: .attributes.general.yearEstablished,
       yearClosed: .attributes.general.yearClosed,
       belongsTo: .attributes.general.relatedSites[] | select(.typeOfRelationship.label == "belongs to") | .listOfSites,
       # contains: .typeOfRelationship.label.["contains"] | select(. != null) | .listOfSites[],
       # contains: .attributes.general.relatedSites[] | if .typeOfRelationship.label != "contains" then "" elif .typeOfRelationship.label == "contains" then .listOfSites else "" end, #this work with https://jqplay.org/ but here an error return
       # contains: .attributes.general.relatedSites[*].[?(@.typeOfRelationship.label == "contents")].listOfSites[*], an error return if it is used this query tested here https://sumiya.page/jpath.html
       # contains: .attributes.general.relatedSites[] | select(.typeOfRelationship.label == "contains") | .listOfSites,
       # formsAClusterWith: .attributes.general.relatedSites[] | select(.typeOfRelationship.label == "forms a cluster with") | .listOfSites[],
       siteType: .attributes.general.siteType,
       protectionLevel: .attributes.general.protectionLevel,
       landUse: .attributes.general.landUse,
       images: .attributes.general.images
      }'
    ),
    site_info = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       created: .created,
       changed: .changed,
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       lterSiteClassification: .projectRelated.lter.lterSiteClassification
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
       accessibleAllYear: .attributes.infrastructure.accessibleAllYear,
       accessType: .attributes.infrastructure.accessType,
       allPartsAccessible: .attributes.infrastructure.allPartsAccessible,
       maintenanceInterval: .attributes.infrastructure.maintenanceInterval,
       permanentPowerSupply: .attributes.infrastructure.permanentPowerSupply,
       operation: .attributes.infrastructure.operation,
       notes: .attributes.infrastructure.notes,
       collection: .attributes.infrastructure.collection,
       data: .attributes.infrastructure.data
      }'
    ),
    site_observedProperties = list(
      path = "sites",
      query = '{
       title: .title,
       uri: "\\(.id.prefix)\\(.id.suffix)",
       geoCoord: .attributes.geographic.coordinates,
       country: .attributes.geographic.country,
       geoElev: .attributes.geographic.elevation,
       observedProperties: .attributes.focusDesignScale.observedProperties
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

# JQ queries for B2share ----
queries_jq_b2s <- list(
  b2share_community_records = list(
    path = "https://b2share.eudat.eu/api/records/?q=community:",
    query = '{
     title: .hits.hits[].metadata.titles[].title,
     created: .hits.hits[].created
    }'
  ),
  b2share_record = list(
    path = "https://b2share.eudat.eu/api/records/",
    query = '{
      title: .metadata.titles[].title,
      created: .created,
      updated: .updated,
      doi: .metadata.DOI,
      community: .metadata | [if (has("community")) then .community else "no_community_declared" end][],
      creators: .metadata | [if (has("creators")) then .creators[].creator_name else "no_creators_declared" end][],
      contact_email: .metadata | [if (has("contact_email")) then .contact_email else "no_contact_email_declared" end][],
      description: .metadata.descriptions[].description,
      ePIC_PID: .metadata.ePIC_PID,
      keywords: .metadata.keywords[],
      open_access: .metadata.open_access,
      license: .metadata | [if (has("open_access")) then .open_access else "no_open_access_declared" end][],
      owners: .metadata.owners[],
      publication_date: .metadata.publication_date,
      publication_state: .metadata.publication_state,
      type: .metadata | [if (has("resource_types")) then .resource_types[].resource_type_general else "no_resource_types_declared" end][],
      files: .files
    }'
  )
)
