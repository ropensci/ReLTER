#' @title createSiteReport
#' @description This function allows to obtain a pdf report of eLTER site selected by DEIMS.ID. All the info are taken from the DEIMS-SDR sites API.
#' @param deimsid A character. It is the DEIMS iD of site make from DEIMS-SDR website. More information about DEIMS iD in this page \url{https://deims.org/docs/deimsid.html}.
#' @return The output of the function is a PDF with boundaries of the site.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @import jsonlite httr tibble sf leaflet reactable
#' @export
#' @examples
#' createSiteReport(deimsid = 'https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe')
#'
### function createSiteReport
createSiteReport <- function(deimsid) {
  siteAffiliations <- ReLTER::getSiteAffiliations(deimsid = deimsid)
  siteContact <- ReLTER::getSiteContact(deimsid = deimsid)
  siteGeneral <- ReLTER::getSiteGeneral(deimsid = deimsid)
  siteEnvCharact <- ReLTER::getSiteEnvCharacts(deimsid = deimsid)
  siteBoundaries <- ReLTER::getSiteBoundaries(deimsid = deimsid)
  siteParameter <- ReLTER::getSiteParameters(deimsid = deimsid)
  siteResearchTopic <- ReLTER::getSiteResearchTopics(deimsid = deimsid)
  siteInfrastructure <- ReLTER::getSiteInfrastructure(deimsid = deimsid)
  siteRelatedResources <- ReLTER::getSiteRelatedResources(deimsid = deimsid)
  
  # Basic info
  siteTitle <- siteAffiliations$title
  siteId <- siteAffiliations$uri
  siteCountry <- siteGeneral$country[[1]]
  siteAbstract <- siteGeneral$generalInfo.abstract
  siteParent <- siteGeneral$generalInfo.hierarchy.parent
  siteManager <- siteContact$generalInfo.siteManager
  siteWebsite <- siteContact$generalInfo.siteUrl
  sitefundingAgency <- siteContact$generalInfo.fundingAgency
  sitemetadataProvider <- siteContact$generalInfo.metadataProvider
  siteOperatingOrganisation <- siteContact$generalInfo.operatingOrganisation
  siteAffiliationsNetwork <- siteAffiliations$affiliation.networks
  siteAffiliationsProjects <- siteAffiliations$affiliation.projects
  siteStatus <- siteGeneral$generalInfo.status.label
  siteKeywords <- siteGeneral$generalInfo.keywords
  siteShortName <- siteGeneral$generalInfo.shortName
  siteType <- siteGeneral$generalInfo.siteType
  siteQRcode <- paste0("eLTER_qrCode_", gsub(' ', '_', siteTitle), ".png")
  
  # Geographic
  siteImagesFileName <- paste0("sites_", gsub(' ', '_', siteTitle), ".png")
  siteElevationAverage <- siteGeneral$geoElev.avg
  siteElevationMin <- siteGeneral$geoElev.min
  siteElevationMax <- siteGeneral$geoElev.max
  siteElevationUnit <- siteGeneral$geoElev.unit
  siteCoordinates <- siteAffiliations$geoCoord
  
  # General Characteristics
  siteObservedProperties <- siteParameter$parameter
  siteStatusLabel <- siteGeneral$generalInfo.status.label
  siteStatusUri <- siteGeneral$generalInfo.status.uri
  siteYearEstablished <- siteGeneral$generalInfo.yearEstablished
  siteYearClosed <- siteGeneral$generalInfo.yearClosed
  
  # Env Characteristics
  siteAirTemperature.avg <- siteEnvCharact$envCharacteristics.airTemperature.avg
  siteAirTemperature.min <- siteEnvCharact$envCharacteristics.airTemperature.min
  siteAirTemperature.max <- siteEnvCharact$envCharacteristics.airTemperature.max
  siteAirTemperature.unit <- siteEnvCharact$envCharacteristics.airTemperature.unit
  sitePrecipitation.annual <- siteEnvCharact$envCharacteristics.precipitation.annual
  sitePrecipitation.min <- siteEnvCharact$envCharacteristics.precipitation.min
  sitePrecipitation.max <- siteEnvCharact$envCharacteristics.precipitation.max
  sitePrecipitation.unit <- siteEnvCharact$envCharacteristics.precipitation.unit
  siteBiogeographicalRegion <- siteEnvCharact$envCharacteristics.biogeographicalRegion
  siteBiome <- siteEnvCharact$envCharacteristics.biome
  siteEcosystemAndLanduse <- siteEnvCharact$envCharacteristics.landforms
  siteEunisHabitat <- siteEnvCharact$envCharacteristics.eunisHabitat
  siteGeoBonBiome <- siteEnvCharact$envCharacteristics.geoBonBiome
  siteGeology <- siteEnvCharact$envCharacteristics.geology
  siteHydrology <- siteEnvCharact$envCharacteristics.hydrology
  siteSoils <- siteEnvCharact$envCharacteristics.soils
  siteVegetation <- siteEnvCharact$envCharacteristics.vegetation
  
  # knitr::Sweave2knitr("vignettes/siteTemplate.Rnw")
  # knitr::knit2pdf(
  #   input = 'vignettes/siteTemplate-knitr.Rnw',
  #   output = 'vignettes/siteTemplate-knitr.tex'
  # )
  # system('open "vignettes/siteTemplate-knitr.pdf"')
  
  # knitr::knit('Untitled.Rmd')
  # xfun::Rscript_call(
  #   rmarkdown::render,
  #   list(input = 'siteTemplate.md', output_format = 'pdf_document')
  # )
  rmarkdown::render("Untitled.Rmd")
}


