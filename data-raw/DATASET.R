setwd('~/Documents/cortez/data-raw')

library(sf)
library(tidyverse)
library(smoothr)
library(usethis)
library(data.table)

places <- read.csv('Places.csv') |>
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) |>
  mutate(across(starts_with('date'), ~ as.Date(.x, format = "%m-%d-%Y"))) |>
  arrange(date_arrive)

ggplot() +
  geom_sf(data = places)

st_write(places, 'places.gpkg', append = FALSE)
usethis::use_data(places, overwrite = TRUE)

################################################################################
## Route of the Western Flyer!!!

# essentially I digitized the map route in the book, and freehand draw it out
# in qgis using points. The points then get attached here.

# these are entered in an odd fashion, I made two geopackages rather than two layers in one gpkg

route <- bind_rows(
  st_read('Trip_part-1.gpkg', quiet = T),
  st_read('Trip_part-2.gpkg', quiet = T) |>
    rename(geom = geometry)
) %>%
  filter(!st_is_empty(.)) |>
  summarise(do_union = FALSE) |>
  st_cast("LINESTRING")

# now we will write out the combined parts, and make sure they look ok in qgis.

st_write(route, 'route-ls-1.gpkg', append = F)
# I re ran this a handful of times (intermediates not saved) and modified the original
# vertices to get an OK result which we will now apply some smoothing to

chai_route <- smoothr::smooth(route, method = "chaikin")
st_write(chai_route, 'route-ls-chai.gpkg', append = F)
unlink('route-ls-1.gpkg')

rm(route)
# create segments to assign to locations later.

chai_route <- st_transform(chai_route, 32612)
segs <- (as.numeric(st_length(chai_route)) / 6436)
sample_points <- st_line_sample(chai_route, n = segs, type = "regular")
split_lines <- lwgeom::st_split(chai_route, sample_points) %>%
  st_collection_extract('LINESTRING')

split_lines <- nngeo::st_segments(split_lines, progress = TRUE)
split_lines <- st_transform(split_lines, 4326) |>
  mutate(ID = 1:n())

st_write(split_lines, 'route-split.gpkg', append = F)
unlink('route-ls-chai.gpkg')

rm(segs, sample_points, chai_route)
# and more iterations on the above to get a product I liked.

## now we need to append the 'days' to the sections of the route.
# part of this data set is about having times for the line segments.


# this will be slighty messy code, but the general idea is we identify the route
# line segment which is closest to a port and then snap dates between the ports.

# identify route directions from the route-ls-chai data set. we do this by
# identifying the nearest 'trip-part' points

route_dates <- places[4:25,'location.english'] |>
  st_drop_geometry() |>
  rename(Destination = location.english) |>
  mutate(
    End = st_nearest_feature(places[4:25,], split_lines)
  ) |>
  mutate(
    Start = lag(End, n = 1L)+1, .before = End,
    Start = case_when(
      Destination == 'Magdalena Bay' ~ 1,
      .default = as.numeric(Start)
      )
  )

split_lines <- data.table(split_lines)
route_dates <- data.table(route_dates)

setkey(split_lines, ID)
setkey(route_dates, Start)


route <- route_dates[split_lines, roll = T] |>
  mutate(Destination = if_else(Start > 2814, 'San Diego', Destination)) |>
  select(Destination, geometry = result) |>
  st_as_sf() |>
  group_by(Destination) |>
  summarize(geometry = st_union(geometry))

st_write(route, 'route.gpkg', append = FALSE)
rm(split_lines)

usethis::use_data(route, overwrite = TRUE)


##### Markers

# these are points discussed in the book, generally in passing.

dat <- read.csv('./species/Site4.csv')
