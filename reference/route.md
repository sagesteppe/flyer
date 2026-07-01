# A stylized route of the Western Flyer on Ricketts and Steinbeck's journey to the Sea of Cortez.

A dataset with one linestring segment per destination. Note that the
route is fictionalized from the book and tweaked to be aesthetically
pleasing, rather than nautically accurate.

## Usage

``` r
route
```

## Format

An sf tibble, with a single attribute that can be joined to the `places`
data set (if desired).

- destination:

  The place name used in the book. Can be joined to
  `places$location_english` to pick up dates of travel.

- geometry:

  An sf linestring, running from the previous destination to the current
  one.

## Examples

``` r
data(route)

places <- sf::st_drop_geometry(places)
route <- dplyr::left_join(
  route,
  sf::st_drop_geometry(places),
  by = c('destination' = 'location_english')
  )


library(ggplot2)
ggplot() +
 geom_sf(data = route, aes(color = date_arrive)) +
 theme_minimal()

```
