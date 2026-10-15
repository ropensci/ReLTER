# ReLTER: An Interface for the eLTER Community

ReLTER provides access to DEIMS-SDR (https://deims.org/), and allows
interaction with data and software implemented by eLTER Research
Infrastructure (RI) thus improving data sharing among European LTER
projects. ReLTER uses the R language to access and interact with the
DEIMS-SDR archive of information shared by the Long Term Ecological
Research (LTER) network. This package grew within eLTER H2020 as a major
project that will help advance the development of European Long-Term
Ecosystem Research Infrastructures (eLTER RI - https://elter-ri.eu). The
ReLTER package functions in particular allow to: - retrieve the
information about entities (e.g. sites, datasets, and activities) shared
by DEIMS-SDR (see e.g. get_site_info function); - interact with the
ODSEurope (maps.opendatascience.eu) starting with the dataset shared by
DEIMS-SDR (https://deims.org/) (see e.g. the get_site_ODS() function); -
use the eLTER site informations to download and crop geospatial data
from other platforms (see e.g. get_site_ODS function()); - improve the
quality of the dataset (see e.g. get_id_worms()). Functions currently
implemented are derived from discussions of the needs among the eLTER
users community. The ReLTER package will continue to follow the progress
of eLTER-RI and evolve, adding new tools and improvements as required.

## See also

Useful links:

- <https://docs.ropensci.org/ReLTER>

- Report bugs at <https://github.com/ropensci/ReLTER/issues>

## Author

**Maintainer**: Alessandro Oggioni <alessandro.oggioni@cnr.it>
([ORCID](https://orcid.org/0000-0002-7997-219X)) \[funder\]

Authors:

- Alessandro Oggioni <alessandro.oggioni@cnr.it>
  ([ORCID](https://orcid.org/0000-0002-7997-219X)) \[funder\]

- Micha Silver <silverm@post.bgu.ac.il>
  ([ORCID](https://orcid.org/0000-0002-1128-1325)) \[contributor\]

- Paolo Tagliolato <tagliolato.a@irea.cnr.it>
  ([ORCID](https://orcid.org/0000-0002-0261-313X)) \[contributor\]

- Luigi Ranghetti <luigi@ranghetti.info>
  ([ORCID](https://orcid.org/0000-0001-6207-5188)) \[contributor\]

Other contributors:

- Allison Horst (Allison reviewed the package (v. 1.0.0) for rOpenSci,
  see https://github.com/ropensci/software-review/issues/485)
  \[reviewer\]

- Will Bolton (Will reviewed the package (v. 1.0.0) for rOpenSci, see
  https://github.com/ropensci/software-review/issues/485) \[reviewer\]

- Mauro Lepore (Mauro was editor for rOpenSci, see
  https://github.com/ropensci/software-review/issues/485) \[editor\]
