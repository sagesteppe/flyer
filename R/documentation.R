#' Places which the Western Flyer passed through on Ricketts and Steinbecks journey to the sea of Cortez.
#'
#' A dataset with place names provided both following the text, and more modern Mexican place names in Spanish.
#' Note that several of the book place names seem somewhat peculiar, but the localities seem well matched to the current names in spanish.
#' @format sf/tibble/dataframe with two date columns.
#' \describe{
#'	\item{location-espanol}{A currently used place name in spanish.}
#'	\item{location-english}{The name of the location used in the book.}
#'	\item{collect}{Boolean. Whether the crew collected specimens there.}
#'	\item{date_arrive}{The date the Western flyer arrived (or passed the place).}
#'	\item{date_depart}{The date the Western flyer departed, or passed the place.}
#'	\item{geometry}{The geometry of the place, not 100% accurate, and often slightly off shore.}
#'	}
#' @examples
#' data(places)
#' plot(places['location.espanol'])
#'
#' library(ggplot2)
#' ggplot() +
#'	 geom_sf(data = places, aes(color = date_arrive))
#'
"places"

#' A stylized route of the Western Flyer on Ricketts and Steinbecks journey to the sea of Cortez.
#'
#' A dataset with a destination and a route to that destination.
#' Note that the route is fictionalized from the book, and modified to be aesthetically pleasing, rather than nautically accurate.
#' @format sf/tibble/dataframe, with a single variable to join to the 'places' data set (if desired).
#' \describe{
#'	\item{Destination}{The name of locations used in the book. Can be used to join to the "places" dataset to get dates of travel.}
#'	\item{geometry}{a sf linestring, from the previous destination to the current one.}
#' }
#'
#' @examples
#' data(route)
#' plot(route)
#'
#' places <- st_drop_geometry(places)
#' route <- left_join(route, st_drop_geometry(places), by = c('Destination' = 'location.english'))
#'
#' plot(route[,6])
#'
#' library(ggplot2)
#' ggplot() +
#'	 geom_sf(data = route, aes(color = date_arrive))
#'

"route"

#' A stylized topographic data set of Baja California and adjacent areas.
#'
#' A stylized dataset of topographic contours for mapping (not analytical) applications.
#' Data were aggregated, contoured, had small pieces were removed, simplified, corner cut, and simplified again.
#' The contours were generated from Robinson, N., Regetz, J., and Guralnick, R. P. (2014). EarthEnv-DEM90: A nearly-global, void-free, multi-scale smoothed, 90m digital elevation model from fused ASTER and SRTM data. ISPRS Journal of Photogrammetry and Remote Sensing, 87:2014, 57-67. Available at http://www.sciencedirect.com/science/article/pii/S0924271613002360.
#' @format sf/tibble/dataframe, with a single variable detailing the elevation in meters.
#' \describe{
#'	\item{Elevation}{Elevation of the contour line in meters.}
#'	\item{geometry}{a sf linestring, topographic contours.}
#' }
#'
#' @examples
#' data(topography)
#' plot(topography)

"topography"

#' A stylized bathymetric data set of the Sea of Cortez and adjacent areas.
#'
#' A stylized dataset of bathymetric contours for mapping (not analytical) applications.
#' Data were aggregated, contoured, had small pieces were removed, simplified, corner cut, and simplified again.
#' The contours were generated from GEBCO Compilation Group (2024) GEBCO 2024 Grid (doi:10.5285/1c44ce99-0a0d-5f4f- e063-7086abc0ea0f).
#' @format sf/tibble/dataframe, with a single variable detailing the elevation in meters.
#' \describe{
#'	\item{Depth}{Depth of the contour line in meters.}
#'	\item{geometry}{a sf linestring, bathymetric contours.}
#' }
#'
#' @examples
#' data(bathymetry)
#' plot(bathymetry)

"bathymetry"


#' Protected areas of Mexico
#'
#' A very minimally simplified data set of protected areas in Mexico, for cartographic purposes.
#' From CONABIO CONANP, (09/2024). 'Federal Protected Natural Areas of Mexico, September 2024', National Commission for the Knowledge and Use of Biodiversity. Mexico City, Mexico.
#' @format sf/tibble/dataframe, with a handful of variables briefly describing each locality.
#' \describe{
#'  \item{name}{Official name of the protected area.}
#'  \item{date.established}{Date that the preserve was established.}
#'  \item{reserve.type}{A locally created type categorizing reserves into four categories. 'Marine' and 'Terrestrial' (only) for area with 100% of their land surface categorized as either category, 'Primarily Marine' for areas with MORE marine than terrestrial areas and 'Primarily Terrestrial' for areas with MORE terrestrial than marine areas.}
#'  \item{geometry}{A simplified multipolygon geometry.}
#' }
#'
#' @examples
#' data(protected)
#' plot(protected)
"protected"

#' Major landcover classes of Western Mexico
#'
#' A simplified data set of landcover types (for global usage) from EarthEnv.
#' Tuanmu, M.-N. and W. Jetz. 2014. A global 1-km consensus land-cover product for biodiversity and ecosystem modeling. Global Ecology and Biogeography 23(9): 1031-1045.
#' @format sf/tibble/dataframe, with a handful of variables briefly describing each locality.
#' \describe{
#'  \item{class}{Name of the class, slightly modified in several instances. }
#'  \item{geometry}{A simplified polygon geometry.}
#' }
#' @examples
#' data(landcover)
#' plot(landcover)
#' use a palette included with the package for a quick aesthetic mapping... TODO.
"landcover"


#' Land and islands of Western Mexico
#'
#' A simplified data set of land, and nations, (for global usage) from Natural Earth.
#' Basically, combined the high res land data set with the minor islands data set.
#' Islands, which do not contain administrative info from Natural Earth were assigned by being 'nearest' feature to a nation.
#' Based on visual inspection the match is good; any mismatch is not my intention - *viva la Mexico* and god bless the USA.
#' "Made with Natural Earth. Free vector and raster map data @ naturalearthdata.com."
#' @format sf/tibble/dataframe, with two multipolygons, one for each of the countries in the area.
#' \describe{
#'  \item{Name}{Name of the country.}
#'  \item{geometry}{A simplified multipolygon geometry.}
#' }
#' @examples
#' data(land)
#' plot(land)
"land"
