# ReLTER 2.1.1 Release Notes

v2.1.1 was released on 27/01/2023

## Minor changes

* Fixes leaflet prob in get_site_MODIS function.

________________________________________________________________________________


# ReLTER 2.1.0 Release Notes

v2.1.0 was released on 25/01/2023

## Changes

* inserted new function for acquire either Land Surface Temperature (LST)
  or Vegetation Index (NDVI) both cropped to an eLTER site boundary;
* outputs enrichment of certain functions with unit of measurement (UOM)
  and labeling with semantic terms (e.g.
  \href{https://dwc.tdwg.org/terms/}{Darwin Core} in the taxon_id_*.R
  or \href{http://vocab.nerc.ac.uk}{NERC} in the get_sos_*.R functions).

________________________________________________________________________________


# ReLTER 2.0.1 Release Notes

v2.0.1 was released on 24/01/2023

## Changes

* Fix vignette issues
* Fix ilter generalinfo
* Fix documentation
* Fix missing urlencode and change documentation. Add support for multilingual country search
* Fix too many http requests

________________________________________________________________________________


# ReLTER 2.0.0 Release Notes

v2.0.0 was released on 30/09/2022

## Changes

* inserted new functions for set an environment for variables of the package,
  for setting and for get the DEIMS-SDR base URL;
* inserted a new function for get version of DEIMS-SDR API based on the
  DEIMS-SDR base URL;
* added an object (queries_jq) containing all the JQ queries (as a list);
* added the new JQ queries accordingly with the structure of the new DEIMS-SDR
  API version;
* deleted the JQ queries call into the get_site_info(), get_activity_info(),
  and get_dataset_info() and substitute with a variable;
* changed the name of the functions containing "parameters" with "observed
  properties";
* added for some functions the life cycle badge;
* acquisition to species occurrences from GBIF, iNaturalist and OBIS;
* harmonisation to the output of species occurrences in eLTER Data Reporting
  Format (v3.1);
* creation the archive (zip) with files following the eLTER Data Reporting
  Format (v3.1);
* composition of file naming convention following the eLTER Data Reporting
  Format (v3.1);
* composition of the object containing the eLTER Data Reporting Format
  (v3.1) tables;
* interaction with Sensor Observations Services (SOS - v2.0) for acquire
  procedure list, procedure info, feature of interest info, observed property
  info, and observations;
* interaction to the Zenodo repository in order to upload record or download
  dataset record;
* substituted the SPARQL package with httr2.

________________________________________________________________________________


# ReLTER 1.1.0 Release Notes

v1.1.0 was released on 15/04/2022

## Major changes

* all the suggestion from rOpenSci reviewers has been evaluated and taken into account in this release;
* revised and linguistically corrected the documentation of package;
* revised and graphically enriched the documentation of the functions;
* created the vignettes of the package;
* new functions (get_site_ODS and taxon_id_worms) has been implemented;
* enriched the information downloaded for datasets and activities from DEIMS-SDR;
* a solution for install package by Docker has been done;
* all the packages used in the `ReLTER` have been referenced in each functions manual.

________________________________________________________________________________

# ReLTER 1.0.0 Release Notes

v1.0.0 was released on 15/11/2021

## Major changes

* all the get_site_xxx functions could be merged into one, with an additional function parameter category;
* all the functions are renamed according with CRAN suggestion;
* first release of vignettes have been done;
* new functions for interactive improvements of taxa names using PESI and WORMS repository have been done;
* function acquires one of eight raster datasets from the European OpenDataScience maps website for a selected DEIMS site have been implemented by @micha-silver;
* a fully review of functions and package description have been done.

## Minor changes

* CITATION file are been created;
* this version has been submitted on rOpenSci.

________________________________________________________________________________

# ReLTER 0.2.0 Release Notes

v0.2.0 was released on 18/10/2021

## Major changes

After the first minor release big improvements are been done:
* all functions have been re-engineered with harmonized metadata, testing and evaluation of error-causing situations,
* all situation where the functions writing something in the local environment have been removed,
* some test to the functions have been added,
* the logo and thumbnails have been created,
* a new function getILTERGeneralInfo have been added to the package.

## Bug Fixes

* all the warnings, generated during installation, have been removed.

________________________________________________________________________________

# ReLTER 0.1.1 Release Notes

v0.1.1 was released on 28/01/2021

## Major changes

After the first test some improvements are been done:
* all functions have been re-tested
* three functions have been added:
1. createQRCode
2. parametersChartPie
3. parametersChartWaffle
* the NEWS.md file has been created

## Minor Changes

* The examples for all functions have been improved

## Bug Fixes

* The problem of installing the package has been resolved

________________________________________________________________________________

# ReLTER 0.1.0 Release Notes

v0.1.0 was released on 28/01/2019

## Major changes

Beta release of ReLTER package!

* Some functions are released within this version of package:
1. getSiteAffiliations
2. getSiteBoundaries
3. getSiteContact
4. getSiteEnvCharacts
5. getSiteRelatedResources
6. getSiteResearchTopics
7. getSiteGeneral
8. getSiteInfrastructure
9. getSiteParameters
10. getNetworkEnvCharacts
11. getNetworkParameters
12. getNetworkResearchTopics
13. getNetworkRelatedResources
14. getNetworkSites
15. getILTERParameters
16. getILTEREnvCharacts
17. getILTERResearchTopics
18. getActivity
19. getDataset
20. mapNetworkPoint
21. produceMapOfSiteFromDEIMS
22. taxonIDPesi
* Documentation and application site have been created.
* The whole package is released on GitHub and available to the developer community.
