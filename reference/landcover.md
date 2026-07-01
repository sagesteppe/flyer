# Major landcover classes of Western Mexico

A simplified data set of landcover types, derived from the global
EarthEnv product. Tuanmu, M.-N. and W. Jetz. 2014. A global 1-km
consensus land-cover product for biodiversity and ecosystem modeling.
Global Ecology and Biogeography 23(9): 1031-1045.

## Usage

``` r
landcover
```

## Format

An sf tibble with one row per landcover polygon.

- class:

  Name of the landcover class (slightly renamed from the source in a few
  cases).

- geometry:

  A simplified polygon geometry.

## Examples

``` r
data(landcover)

library(ggplot2)
ggplot() +
 geom_sf(data = landcover, aes(fill = class)) +
 theme_minimal()
```
