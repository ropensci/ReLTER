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

* Some fuctions are relesed whitin this version of package:
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
