% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/documentation.R
\docType{data}
\name{topography}
\alias{topography}
\title{A stylized topographic data set of Baja California and adjacent areas.}
\format{
sf/tibble/dataframe, with a single variable detailing the elevation in meters.
\describe{
\item{Elevation}{Elevation of the contour line in meters.}
\item{geometry}{a sf linestring, topographic contours.}
}
}
\usage{
topography
}
\description{
A stylized dataset of topographic contours for mapping (not analytical) applications.
Data were aggregated, contoured, had small pieces were removed, simplified, corner cut, and simplified again.
The contours were generated from Robinson, N., Regetz, J., and Guralnick, R. P. (2014). EarthEnv-DEM90: A nearly-global, void-free, multi-scale smoothed, 90m digital elevation model from fused ASTER and SRTM data. ISPRS Journal of Photogrammetry and Remote Sensing, 87:2014, 57-67. Available at http://www.sciencedirect.com/science/article/pii/S0924271613002360.
}
\examples{
data(topography)

library(ggplot2)
ggplot() +
 geom_sf(data = topography, aes(color = elevation)) +
 theme_minimal()
}
\keyword{datasets}
