# A stylized bathymetric data set of the Sea of Cortez and adjacent areas.

A stylized dataset of bathymetric contours, intended for mapping (not
analytical) applications. The source raster was aggregated, contoured,
filtered for small pieces, simplified, corner-cut, and simplified again.
The contours were generated from GEBCO Compilation Group (2024) GEBCO
2024 Grid (doi:10.5285/1c44ce99-0a0d-5f4f-e063-7086abc0ea0f).

## Usage

``` r
bathymetry
```

## Format

An sf tibble, with a single attribute giving elevation in meters (depth
is stored as negative elevation).

- elevation:

  Elevation of the contour line, in meters – negative values below sea
  level.

- geometry:

  An sf linestring – a bathymetric contour.

## Examples

``` r
data(bathymetry)

library(ggplot2)
ggplot() +
 geom_sf(data = bathymetry, aes(color = elevation)) +
 theme_minimal()
```
