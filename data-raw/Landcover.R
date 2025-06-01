library(terra)
library(sf)
library(rnaturalearth)
library(tidyverse)

setwd('~/Documents/flyer/data-raw')
bb <- st_as_sfc(
  st_bbox(c(xmin = -121, xmax = -103, ymin = 19, ymax = 35.3),
          crs = st_crs(4326)
          )
  )
cntr <- ne_countries(type = "countries", scale = "large") |>
  st_make_valid() |>
  st_crop(bb) |>
  st_union()

lkp_tab <- data.frame(
  class = c('Evergreen Deciduous Needleleaf Forest', 'Evergreen Broadleaf Forest',
            'Deciduous Broadleaf Forest', 'Mixed/Other Forest',
            'Desert & Shrublands', 'Herbaceous Vegetation',
            'Cultivated and Managed Vegetation', 'Regularly Flooded Vegetation',
            'Urban/Developed', # 'Snow/Ice',
            'Barren', 'Open Water'),
  number = 1:11
) # we will want to reduce a few of these... classes. Ice obviously, regularly flooded, http://127.0.0.1:45169/graphics/af09a149-99d8-4082-a5a4-16af460b0d3f.pngand collapse other trees?


f <- list.files('./landcover')
f <- f[order(as.numeric(gsub('[a-z]|_|[.]', '', f)))]
r <- rast(file.path('./landcover', f))
r <- crop(r, vect(bb))
r <- mask(r, vect(cntr))

r <- subset(r,'consensus_full_class_10', negate= TRUE )
r_summary <- app(r, which.max)

r_summary <- aggregate(r_summary, fact = 3, fun = 'modal')
f <- focal(r_summary, w=5, fun="modal", na.rm = TRUE)

v <- as.polygons(f) |>
  st_as_sf() |>
  st_cast('MULTIPOLYGON')

st_is_valid(v)

format(object.size(v), units = 'MB')
nc_simp <- rmapshaper::ms_simplify(v, keep = 0.5) |>
  smoothr::smooth(method = "chaikin", refinements = 1) |>
  rmapshaper::ms_simplify(keep = 0.5) |>
  st_cast('POLYGON')|>
  st_make_valid()

format(object.size(nc_simp), 'MB')

ggplot() +
  geom_sf(data = nc_simp)

nc_simp <- st_intersection(nc_simp, cntr)
nc_simp <- nc_simp[!st_is_empty(nc_simp),]
nc_simp <- st_collection_extract(nc_simp, type = 'POLYGON')
nc_simp <- nc_simp[ st_geometry_type(nc_simp) %in% c('POLYGON', 'MULTIPOLYGON'), ]
nc_simp <- st_make_valid(nc_simp)

nc_simp %>%
  filter(st_is_valid(.)) %>%
  ggplot(., aes(fill = focal_modal)) +
  geom_sf(, color = NA)

all(st_is_valid(nc_simp))# SO EVERYTHING IS GOOD SO FAR !!!

landcover <- nc_simp |>
  select(number = focal_modal) |>
  left_join(lkp_tab) |>
  select(-number)

row.names(landcover) <- 1:nrow(landcover)

st_write(landcover, '../docs/landcover.gpkg')
usethis::use_data(landcover, overwrite = TRUE)


class = c('Evergreen Deciduous Needleleaf Forest', 'Evergreen Broadleaf Forest',
          'Deciduous Broadleaf Forest', 'Mixed/Other Forest',
          'Desert & Shrublands', 'Herbaceous Vegetation',
          'Cultivated and Managed Vegetation', 'Regularly Flooded Vegetation',
          'Urban/Developed',  'Snow/Ice',
          'Barren', 'Open Water')

lc_pal <- c(
  '#023B0A', '#4B6C4D', '#709775', '#1B512D', '#E2D4BA',  '#8FB996',
  '#069E2D',  '#95E06C', '#8D8EA5',  '#FFFBFE', '#CBC0D3', '#37718E')
names(lc_pal) <- class

usethis::use_data(lc_pal, overwrite = TRUE)

lc_pal <- set_names(
  data.frame(
    names(lc_pal),
    unname(lc_pal)
  ),
  c('class', 'hex')
)

write.csv(lc_pal, '../docs/landcover_palette.csv', row.names = FALSE)
