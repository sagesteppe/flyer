# An interactive map

## Introduction

The `ExploreDataSets` vignette walks through building static `ggplot2`
maps of the collection route. One of the stated advantages of the data
set is that it follows a sequential series of events, which lends itself
just as well to an interactive treatment — a reader can pan the Sea of
Cortez, click a stop to see its name in Spanish and English, and trace
the *Western Flyer*’s path day by day.

Here we build a `leaflet` map with a time slider (via
[`leaflet.extras2::addTimeslider`](https://trafficonese.github.io/leaflet.extras2/reference/addTimeslider.html))
that reveals the collection stops progressively by arrival date. The
full route stays drawn in the background as visual context, colored by
arrival date, while the play/scrub control lets a reader replay the
expedition day by day.

``` r

library(flyer)
library(sf)
library(dplyr)
library(leaflet)
library(leaflet.extras2)
```

## Preparing the data

The `route` object only carries a `destination` field, so we join it to
`places` to bring the arrival date onto each segment — the same pattern
used in the static vignette.

``` r

route <- left_join(
  route,
  st_drop_geometry(places),
  by = c('destination' = 'location_english')
) |>
  relocate(geometry, .after = last_col())
```

## Building the map

We use a continuous palette on `date_arrive` for the route segments, and
a small handful of quantile-based tick marks for the legend so the
labels stay readable.

``` r

date_domain <- as.numeric(route$date_arrive)
pal <- colorNumeric(palette = 'viridis', domain = date_domain, na.color = 'transparent')

legend_breaks <- quantile(date_domain, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)
legend_labels <- format(as.Date(legend_breaks, origin = '1970-01-01'), '%b %d')
```

A few notes on interaction: `label` is the small text that appears on
*hover*, `popup` is the richer HTML card that appears on *click*, and
the time slider (top-right of the map, once knit) is a two-handle range
slider that filters which markers are shown between a start and end
date.

`addTimeslider` expects a single time attribute per feature and — under
the hood — caps the slider at the count of *distinct* times minus one.
Because a few stops share an arrival date (e.g. two places both dated
March 19), we synthesize per-feature unique timestamps by adding a small
seconds offset within same-date groups. That keeps the slider from
stranding the last few features. We also set `timeStrLength = 10` so
only the date portion is displayed in the slider readout.

``` r

places <- places |>
  arrange(date_arrive) |>
  group_by(date_arrive) |>
  mutate(seq = row_number() - 1L) |>
  ungroup() |>
  mutate(
    arrive_str = format(date_arrive, '%b %d, %Y'),
    depart_str = format(date_depart, '%b %d, %Y'),
    stay = ifelse(
      date_arrive == date_depart,
      arrive_str,
      paste(arrive_str, '–', depart_str)
    ),
    time = format(as.POSIXct(date_arrive) + seq, '%Y-%m-%dT%H:%M:%S')
  ) |>
  select(-seq)

leaflet(width = '100%', height = '650px') |>
  addProviderTiles(providers$Esri.OceanBasemap) |>
  addPolylines(
    data = route,
    color = ~pal(as.numeric(date_arrive)),
    weight = 3,
    opacity = 0.9,
    label = ~paste0(destination, ' · ', format(date_arrive, '%b %d'))
  ) |>
  addTimeslider(
    data = places,
    radius = 5,
    color = '#222823',
    fillColor = '#D35269',
    fillOpacity = 0.9,
    weight = 1,
    label = ~paste0(location_english, ' · ', format(date_arrive, '%b %d')),
    popup = ~paste0(
      '<b>', location_english, '</b><br/>',
      '<i>', location_espanol, '</i><br/>',
      '<hr style="margin:4px 0" />',
      'Arrived: ', arrive_str, '<br/>',
      'Departed: ', depart_str, '<br/>',
      'Stay: ', stay, '<br/>',
      'Collection site: ', ifelse(collect, 'yes', 'no')
    ),
    options = timesliderOptions(
      timeAttribute = 'time',
      timeStrLength = 10,
      range = TRUE,
      alwaysShowDate = TRUE,
      position = 'topright'
    )
  ) |>
  addLegend(
    position = 'bottomright',
    colors = pal(legend_breaks),
    labels = legend_labels,
    title = 'Date arrived',
    opacity = 0.9
  )
#> Loading required namespace: yyjsonr
```
