# Land and islands of Western Mexico

A simplified data set of land polygons and their associated nations,
adapted from Natural Earth's global products. Built by combining the
high-resolution land data set with the minor islands data set. Islands
that lacked administrative info in Natural Earth were assigned to their
nearest nation. Based on visual inspection the match is good; any
mismatch is not intentional – *viva la Mexico* and god bless the USA.
"Made with Natural Earth. Free vector and raster map data @
naturalearthdata.com."

## Usage

``` r
land
```

## Format

An sf tibble, with one multipolygon per country in the area.

- name:

  Name of the country.

- geometry:

  A simplified multipolygon geometry.

## Examples

``` r
data(land)

library(ggplot2)
ggplot() +
 geom_sf(data = land, aes(fill = name)) +
 theme_minimal()
```
