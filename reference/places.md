# Places the Western Flyer passed through on Ricketts and Steinbeck's journey to the Sea of Cortez.

A dataset providing both the place names used in the book and the modern
Mexican equivalents in Spanish. Several of the book place names are a
bit peculiar, but the localities match up well with the current Spanish
names.

## Usage

``` r
places
```

## Format

An sf tibble with one row per stop.

- location_espanol:

  The current place name, in Spanish.

- location_english:

  The place name used in the book.

- collect:

  Logical. Whether the crew collected specimens there.

- date_arrive:

  The date the Western Flyer arrived (or passed the place).

- date_depart:

  The date the Western Flyer departed (or passed the place).

- geometry:

  The point geometry – not perfectly accurate, and often slightly
  offshore.

## Examples

``` r
data(places)

library(ggplot2)
ggplot() +
 geom_sf(data = places, aes(color = date_arrive)) +
 theme_minimal()

```
