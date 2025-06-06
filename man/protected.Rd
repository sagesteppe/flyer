% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/documentation.R
\docType{data}
\name{protected}
\alias{protected}
\title{Protected areas of Mexico}
\format{
sf/tibble/dataframe, with a handful of variables briefly describing each locality.
\describe{
\item{name}{Official name of the protected area.}
\item{date.established}{Date that the preserve was established.}
\item{reserve.type}{A locally created type categorizing reserves into four categories. 'Marine' and 'Terrestrial' (only) for area with 100\% of their land surface categorized as either category, 'Primarily Marine' for areas with MORE marine than terrestrial areas and 'Primarily Terrestrial' for areas with MORE terrestrial than marine areas.}
\item{geometry}{A simplified multipolygon geometry.}
}
}
\usage{
protected
}
\description{
A very minimally simplified data set of protected areas in Mexico, for cartographic purposes.
From CONABIO CONANP, (09/2024). 'Federal Protected Natural Areas of Mexico, September 2024', National Commission for the Knowledge and Use of Biodiversity. Mexico City, Mexico.
}
\examples{
data(protected)

library(ggplot2)
ggplot() +
 geom_sf(data = protected, aes(fill = reserve_type)) +
 theme_minimal()
}
\keyword{datasets}
