# A dark-mode map

## Introduction

The `ExploreDataSets` vignette builds a static map with a light
“nautical” theme. Here we redo that map in a dark mode, borrowing the
palette from the package’s hex sticker: a navy night sky, warm orange
land, a purple route, and star markers for the collection stops.

``` r

library(flyer)
library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggspatial)
```

## The map

Layered “from the bottom to the top”: bathymetry contours in cool blues,
land in warm oranges (`gold` border on a lighter fill), topography
contours in mint, the route as a purple gradient by arrival date, and
the places as gold star-glyphs with cream labels on a dark backing.

``` r

ggplot() +

  # bathymetry from deep ocean to shallower shelves
  geom_sf(
    data = bathymetry,
    aes(color = elevation),
    lwd = 0.35
  ) +
  scale_color_gradient(
    'Depth (m)',
    low = sticker_pal$night,
    high = sticker_pal$ocean_hi,
    guide = 'none'
  ) +

  # land -- warm orange fill with a coral border
  geom_sf(
    data = land,
    fill = sticker_pal$land_light,
    color = sticker_pal$coral,
    linewidth = 0.25
  ) +

  # topography -- mint to cream gradient by elevation
  ggnewscale::new_scale_color() +
  geom_sf(
    data = topography,
    aes(color = elevation),
    linewidth = 0.45
  ) +
  scale_color_gradient(
    'Elevation (m)',
    low = sticker_pal$cream,
    high = alpha(sticker_pal$mint, 0.55),
    guide = 'none'
  ) +

  # route -- purple gradient along arrival date
  ggnewscale::new_scale_color() +
  geom_sf(
    data = route,
    aes(color = date_arrive),
    linewidth = 0.8
  ) +
  scale_color_gradient(
    'Date',
    low = sticker_pal$purple,
    high = sticker_pal$lavender,
    breaks = date_scale,
    labels = date_lbls
  ) +

  # places -- rendered as star glyphs
  geom_sf_text(
    data = places,
    label = '★',   # unicode filled star
    color = sticker_pal$star_pink,
    size = 6
  ) +
  ggrepel::geom_label_repel(
    data = places,
    aes(label = location_espanol, geometry = geometry),
    stat = 'sf_coordinates',
    size = 2.4,
    color = sticker_pal$cream,
    fill = alpha(sticker_pal$night, 0.75),
    label.size = NA,
    label.padding = 0.15,
    segment.color = alpha(sticker_pal$star_gold, 0.6)
  ) +

  # ambiance
  coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]), crs = 4326) +
  annotation_scale(
    bar_cols = c(sticker_pal$gold, sticker_pal$night),
    text_col = sticker_pal$cream,
    line_col = sticker_pal$gold
  ) +
  annotation_north_arrow(
    which_north = 'true',
    style = north_arrow_nautical(
      fill = c(sticker_pal$cream, sticker_pal$night),
      line_col = sticker_pal$gold,
      text_col = sticker_pal$cream
    )
  ) +
  theme_nautical_night() +
  labs(
    x = NULL, y = NULL,
    title = 'flyer',
    subtitle = 'Sea of Cortez, spring 1940',
    caption = '"it is advisable to look from the tide pool to the stars\nand back to the tidepool again"'
  )
```

![](NightMap_files/figure-html/night%20map-1.png)
