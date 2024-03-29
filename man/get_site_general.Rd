% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_site_general.R
\name{get_site_general}
\alias{get_site_general}
\title{eLTER get_site_general function}
\usage{
get_site_general(deimsid)
}
\arguments{
\item{deimsid}{A \code{character}. The DEIMS ID of the site from
DEIMS-SDR website. DEIMS ID information
\href{https://deims.org/docs/deimsid.html}{here}.}
}
\value{
The output of the function is a \code{tibble} with main features of the
site and the general information, such as: abstract, purpose,
status, yearEstablished, yearClosed, hierarchy, siteName, short name, site
type, protection level, images.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
This internal function obtains general information
about an eLTER site through the DEIMS-SDR sites API.
}
\author{
Alessandro Oggioni, phD (2020) \email{oggioni.a@irea.cnr.it}
}
\keyword{internal}
