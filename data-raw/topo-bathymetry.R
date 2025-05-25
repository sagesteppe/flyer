setwd('~/Documents/flyer/data-raw')

library(sf)
library(terra)
library(tidyverse)
library(smoothr)
library(rnaturalearth)


p <- '~/Documents/DEM4flyr'
f <- file.path(p, list.files(p, pattern = '.bil$'))
r <- mosaic(sprc(f))
r <- crop(r, ext(-121, -103, 19, 35.3))

r <- aggregate(r, fact = 2, fun = 'mean')

# now cut off everything north of San Fran. our northern limit is Santa Cruz :)

v <- as.contour(r, maxcell = 7.5e6) # fewer samples, fewer lines! more generalized

sf_ver <- st_as_sf(v) |>
  filter(level != 0) |>   # these are near the shore, remove them.
  st_make_valid() |> # make all gucci mane
  st_cast('LINESTRING') |>
  mutate(ID = 1:n()) # add some ID's to play with small polys in QGIS and sort it out.

level500 <- sf_ver$level==500
areas <-  units::set_units(st_length(sf_ver), 'km')
indx <- which(level500==TRUE & areas < units::set_units(15, 'km'))

sf_ver <- sf_ver[-indx,]

rm(level500, indx, areas)

level1k <- sf_ver$level==1000
areas <-  units::set_units(st_length(sf_ver), 'km')
indx <- which(level1k==TRUE & areas < units::set_units(10, 'km'))

sf_ver <- sf_ver[-indx,]

# now the last areas .
level1k <- sf_ver$level>=1000
areas <-  units::set_units(st_length(sf_ver), 'km')
indx <- which(level1k==TRUE & areas < units::set_units(5, 'km'))

rm(level1k, indx, areas)

# if level is 500 and area < 10 hectares ( number currently made up) remove
# if level is >1000 & NOT WITHIN 500 then remove < 10 hectares too

# first drastically simplify
sf_ver_1 <- rmapshaper::ms_simplify(sf_ver, keep = 0.05)
sf_ver_1 <- sf_ver_1[!st_is_empty(sf_ver_1),] # drop removed features.

sf_ver_2 <- smoothr::smooth(sf_ver_1, method = "chaikin") # cut the corners slightly
topography <- rmapshaper::ms_simplify(sf_ver_2, keep = 0.4) # de-densify

object.size(topography) / object.size(sf_ver)
format(object.size(topography), units = 'MB')

topography <- select(topography, -ID) |>
  rename(Elevation = level) |>
  st_as_sf()
usethis::use_data(topography, overwrite = TRUE)

rm(sf_ver, r, sf_ver_1, topography, v, f, p)


#################################################

# now perform similar process for the bathymetry.
p <- '~/Documents/bathym4flyr'
r <- rast(file.path(p, list.files(p, recursive = T, pattern = 'tif$')))
cntr <- ne_countries(type = "countries", scale = "large")

r <- aggregate(r, fact = 2, fun = 'mean')

plot(r)
# some of these areas are marked as super deep. Whether they are an artifact or not
# they are a bit extreme for our visualization... let's reduce them

msk <- ifel(r < -4300, NA, r)
r <- mask(r, msk, updatevalue = -4300)
plot(r)

r <- mask(r, cntr, inverse = TRUE)
v <- as.contour(r, maxcell = 1.5e6)
v <- crop(v, ext(-121, -103, 19, 35.3))
v <- st_as_sf(v)

sf_ver <- st_as_sf(v) |>
  filter(level != 0) |>   # these are near the shore, remove them.
  st_make_valid() |> # make all gucci mane
  st_cast('LINESTRING') |>
  mutate(ID = 1:n()) # add some ID's to play with small polys in QGIS and sort it out.

bathymetry <- sf_ver[ st_length(sf_ver) > units::set_units(15, 'km'), ] |>
  select(-ID) |>
  rename(Elevation = level) |>
  st_as_sf()

format(object.size(bathymetry), units = 'MB')
usethis::use_data(bathymetry, overwrite = TRUE)


######################################################

cntr <- ne_countries(type = "countries", scale = "large")
cntr <- st_make_valid(cntr)

bb <-st_as_sfc(
  st_bbox(
    c(xmin = -121, xmax = -103, ymin = 19, ymax = 35.3),  crs = st_crs(4326)
  )
)

cntr <- st_crop(cntr, bb) |>
  select(Name = name_sort)

plot(cntr)

mis <- ne_download(scale = 10, type = "minor_islands", category = "physical")
mis <- st_crop(mis, bb) |>
  select(geometry)  %>%
  mutate(
    Name = st_nearest_feature(., cntr),
    Name = if_else(Name == 2, 'Mexico', 'United States of America'), .before = geometry,
    )

land <- bind_rows(cntr, mis) |>
  group_by(Name) |>
  reframe(geometry = st_union(geometry)) |>
  st_as_sf()

format(object.size(land), units = 'MB')
usethis::use_data(land, overwrite = TRUE)

rm(mis, cntr)
