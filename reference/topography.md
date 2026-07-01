# A stylized topographic data set of Baja California and adjacent areas.

A stylized dataset of topographic contours, intended for mapping (not
analytical) applications. The source raster was aggregated, contoured,
filtered for small pieces, simplified, corner-cut, and simplified again.
The contours were generated from Robinson, N., Regetz, J., and
Guralnick, R. P. (2014). EarthEnv-DEM90: A nearly-global, void-free,
multi-scale smoothed, 90m digital elevation model from fused ASTER and
SRTM data. ISPRS Journal of Photogrammetry and Remote Sensing, 87:2014,
57-67. Available at
http://www.sciencedirect.com/science/article/pii/S0924271613002360.

## Usage

``` r
topography
```

## Format

An sf tibble, with a single attribute giving elevation in meters.

- elevation:

  Elevation of the contour line, in meters.

- geometry:

  An sf linestring – a topographic contour.

## Examples

``` r
data(topography)

library(ggplot2)
ggplot() +
 geom_sf(data = topography, aes(color = elevation)) +
 theme_minimal()
```
