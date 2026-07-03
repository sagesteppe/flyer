#' Places the Western Flyer passed through on Ricketts and Steinbeck's journey to the Sea of Cortez.
#'
#' A dataset providing both the place names used in the book and the modern Mexican equivalents in Spanish.
#' Several of the book place names are a bit peculiar, but the localities match up well with the current Spanish names.
#' @format An sf tibble with one row per stop.
#' \describe{
#'	\item{location_espanol}{The current place name, in Spanish.}
#'	\item{location_english}{The place name used in the book.}
#'	\item{collect}{Logical. Whether the crew collected specimens there.}
#'	\item{date_arrive}{The date the Western Flyer arrived (or passed the place).}
#'	\item{date_depart}{The date the Western Flyer departed (or passed the place).}
#'	\item{geometry}{The point geometry -- not perfectly accurate, and often slightly offshore.}
#'	}
#' @examples
#' data(places)
#'
#' library(ggplot2)
#' ggplot() +
#'	 geom_sf(data = places, aes(color = date_arrive)) +
#'	 theme_minimal()
#'

"places"

#' A stylized route of the Western Flyer on Ricketts and Steinbeck's journey to the Sea of Cortez.
#'
#' A dataset with one linestring segment per destination.
#' Note that the route is fictionalized from the book and tweaked to be aesthetically pleasing, rather than nautically accurate.
#' @format An sf tibble, with a single attribute that can be joined to the `places` data set (if desired).
#' \describe{
#'	\item{destination}{The place name used in the book. Can be joined to `places$location_english` to pick up dates of travel.}
#'	\item{geometry}{An sf linestring, running from the previous destination to the current one.}
#' }
#'
#' @examples
#' data(route)
#'
#' places <- sf::st_drop_geometry(places)
#' route <- dplyr::left_join(
#'   route,
#'   sf::st_drop_geometry(places),
#'   by = c('destination' = 'location_english')
#'   )
#'
#'
#' library(ggplot2)
#' ggplot() +
#'	 geom_sf(data = route, aes(color = date_arrive)) +
#'	 theme_minimal()
#'

"route"

#' A stylized topographic data set of Baja California and adjacent areas.
#'
#' A stylized dataset of topographic contours, intended for mapping (not analytical) applications.
#' The source raster was aggregated, contoured, filtered for small pieces, simplified, corner-cut, and simplified again.
#' The contours were generated from Robinson, N., Regetz, J., and Guralnick, R. P. (2014). EarthEnv-DEM90: A nearly-global, void-free, multi-scale smoothed, 90m digital elevation model from fused ASTER and SRTM data. ISPRS Journal of Photogrammetry and Remote Sensing, 87:2014, 57-67. Available at http://www.sciencedirect.com/science/article/pii/S0924271613002360.
#' @format An sf tibble, with a single attribute giving elevation in meters.
#' \describe{
#'	\item{elevation}{Elevation of the contour line, in meters.}
#'	\item{geometry}{An sf linestring -- a topographic contour.}
#' }
#'
#' @examples
#' data(topography)
#'
#' library(ggplot2)
#' ggplot() +
#'	 geom_sf(data = topography, aes(color = elevation)) +
#'	 theme_minimal()

"topography"

#' A stylized bathymetric data set of the Sea of Cortez and adjacent areas.
#'
#' A stylized dataset of bathymetric contours, intended for mapping (not analytical) applications.
#' The source raster was aggregated, contoured, filtered for small pieces, simplified, corner-cut, and simplified again.
#' The contours were generated from GEBCO Compilation Group (2024) GEBCO 2024 Grid (doi:10.5285/1c44ce99-0a0d-5f4f-e063-7086abc0ea0f).
#' @format An sf tibble, with a single attribute giving elevation in meters (depth is stored as negative elevation).
#' \describe{
#'	\item{elevation}{Elevation of the contour line, in meters -- negative values below sea level.}
#'	\item{geometry}{An sf linestring -- a bathymetric contour.}
#' }
#'
#' @examples
#' data(bathymetry)
#'
#' library(ggplot2)
#' ggplot() +
#'	 geom_sf(data = bathymetry, aes(color = elevation)) +
#'	 theme_minimal()

"bathymetry"


#' Protected areas of Mexico
#'
#' A very lightly simplified data set of protected areas in Mexico, for cartographic purposes.
#' From CONABIO CONANP, (09/2024). 'Federal Protected Natural Areas of Mexico, September 2024', National Commission for the Knowledge and Use of Biodiversity. Mexico City, Mexico.
#' @format An sf tibble, with a handful of attributes briefly describing each protected area.
#' \describe{
#'  \item{name}{Official name of the protected area.}
#'  \item{date_established}{Date the preserve was established.}
#'  \item{reserve_type}{A locally defined category assigned to each reserve. `Marine` and `Terrestrial` are reserved for areas with 100pct of their surface in that category; `Primarily Marine` and `Primarily Terrestrial` are used for mixed areas, based on which surface type dominates.}
#'  \item{geometry}{A simplified multipolygon geometry.}
#' }
#'
#' @examples
#' data(protected)
#'
#' library(ggplot2)
#' ggplot() +
#'	 geom_sf(data = protected, aes(fill = reserve_type)) +
#'	 theme_minimal()

"protected"

#' Major landcover classes of Western Mexico
#'
#' A simplified data set of landcover types, derived from the global EarthEnv product.
#' Tuanmu, M.-N. and W. Jetz. 2014. A global 1-km consensus land-cover product for biodiversity and ecosystem modeling. Global Ecology and Biogeography 23(9): 1031-1045.
#' @format An sf tibble with one row per landcover polygon.
#' \describe{
#'  \item{class}{Name of the landcover class (slightly renamed from the source in a few cases).}
#'  \item{geometry}{A simplified polygon geometry.}
#' }
#' @examples
#' data(landcover)
#'
#' library(ggplot2)
#' ggplot() +
#'	 geom_sf(data = landcover, aes(fill = class)) +
#'	 theme_minimal()

"landcover"


#' Land and islands of Western Mexico
#'
#' A simplified data set of land polygons and their associated nations, adapted from Natural Earth's global products.
#' Built by combining the high-resolution land data set with the minor islands data set.
#' Islands that lacked administrative info in Natural Earth were assigned to their nearest nation.
#' Based on visual inspection the match is good; any mismatch is not intentional -- *viva la Mexico* and god bless the USA.
#' "Made with Natural Earth. Free vector and raster map data @ naturalearthdata.com."
#' @format An sf tibble, with one multipolygon per country in the area.
#' \describe{
#'  \item{name}{Name of the country.}
#'  \item{geometry}{A simplified multipolygon geometry.}
#' }
#' @examples
#' data(land)
#'
#' library(ggplot2)
#' ggplot() +
#'	 geom_sf(data = land, aes(fill = name)) +
#'	 theme_minimal()

"land"


#' Color palette for landcover data
#'
#' A hand-picked set of hex colors (via coolors.co) intended to pair reasonably with the `landcover` classes.
#' @format A named character vector of hex color codes, with names matching the levels of `landcover$class`.
#' @examples
#' data(lc_pal)
#' data(landcover)
#'
#'library(ggplot2)
#' ggplot() +
#' geom_sf(data = landcover, aes(fill = class)) +
#'  scale_fill_manual(values = lc_pal) +
#'  theme_minimal()

"lc_pal"

#' Species lists for collection sites
#' 
#' Species lists for collections sites from Brusco 2020 ('the manuscipt')
#' @format an sf tibble with one row per collection
#' \describe{
#'  \item{scientific_name}{Name of the collection per the manuscript}
#'  \item{type_specimen}{Marked as a type based on the manuscript}
#'  \item{synonyms}{Synonym names mentioned in the manuscript}
#'  \item{other_notes}{notes on the collection from the manuscript}
#'  \item{questionable_id}{notes from the manuscript on identification status}
#'  \item{site}{Collection site name per the manuscript}
#'  \item{gbif_usage_key}{GBIF usage key for integration with GBIF}
#'  \item{gbif_scientific_name}{GBIF used scientific name for the collection}
#'  \item{gbif_match_type}{How the 'scientific_name' was resolved with GBIF}
#'  \item{gbif_status}{Whether GBIF recognizes a particular name applied to the collection}
#'  \item{gbif_url}{Hyperlink to the GBIF page for the resolved entry.}
#'  \item{collection_site}{Collection site name per the manuscript}
#'  \item{location_english}{flyer package name for the site from 'places'}
#' }
#' @examples
#' data(collections)
#' head(collections)
#' 
#' ## if you want to make it spatial
#' data(places)
#' collections_sf <- dplyr::left_join(
#'      collections, 
#'      dplyr::select(places, location_english, geometry),
#'      by = 'location_english'
#' ) |>
#'   sf::st_as_sf()
#' 

"collections"
