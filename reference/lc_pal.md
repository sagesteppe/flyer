# Color palette for landcover data

A hand-picked set of hex colors (via coolors.co) intended to pair
reasonably with the `landcover` classes.

## Usage

``` r
lc_pal
```

## Format

A named character vector of hex color codes, with names matching the
levels of `landcover$class`.

## Examples

``` r
data(lc_pal)
data(landcover)

library(ggplot2)
ggplot() +
geom_sf(data = landcover, aes(fill = class)) +
 scale_fill_manual(values = lc_pal) +
 theme_minimal()
```
