library(terra)
library(sf)
library(rnaturalearth)
library(tidyverse)

setwd('~/Documents/cortez/data-raw')
bb <- st_as_sfc(
  st_bbox(c(xmin = -121, xmax = -103, ymin = 19, ymax = 35.3),
          crs = st_crs(4326)
          )
  )
cntr <- ne_countries(type = "countries", scale = "large") |>
  st_make_valid()

lkp_tab <- data.frame(
  class = c('Evergreen Deciduous Needleleaf Forest', 'Evergreen Broadleaf Forest',
            'Deciduous Broadleaf Forest', 'Mixed/Other Forest',
            'Shrubs', 'Herbaceous Vegetation',
            'Cultivated and Managed Vegetation', 'Regularly Flooded Vegetation',
            'Urban/Developed', 'Snow/Ice',
            'Barren', 'Open Water'),
  number = 1:12
) # we will want to reduce a few of these... classes. Ice obviously, regularly flooded, http://127.0.0.1:45169/graphics/af09a149-99d8-4082-a5a4-16af460b0d3f.pngand collapse other trees?

r <- rast(file.path('./landcover', list.files('./landcover')))
r <- crop(r, bb)
r <- mask(r, cntr)


r <- subset(r,'consensus_full_class_10', negate= TRUE )
r_summary <- app(r, which.max)

r_summary <- aggregate(r_summary, fact = 3, fun = 'modal')
f <- focal(r_summary, w=5, fun="modal", na.rm = TRUE)

v <- as.polygons(f) |>
  st_as_sf() |>
  st_cast('MULTIPOLYGON')

st_is_valid(v)
st_write(v, 'no_change.gpkg', append = TRUE)

format(object.size(v), units = 'MB')
nc_simp <- rmapshaper::ms_simplify(v, keep = 0.5) |>
  smoothr::smooth(method = "chaikin", refinements = 1) |>
  rmapshaper::ms_simplify(keep = 0.5) |>
  st_cast('POLYGON')|>
  st_make_valid()

format(object.size(nc_simp), 'MB')

nc_simp <- st_intersection(nc_simp, cntr)
nc_simp <- nc_simp[!st_is_empty(nc_simp),]
nc_simp <- nc_simp[ st_geometry_type(nc_simp) %in% c('POLYGON', 'MULTIPOLYGON'), ]


nc_simp %>%
  filter(st_is_valid(.)) %>%
  ggplot(., aes(fill = factor(focal_modal))) +
  geom_sf(, color = NA)

any(st_is_valid(nc_simp)==F) # SO EVERYTHING IS GOOD SO FAR !!!

landcover <- nc_simp |>
  select(number = focal_modal) |>
  left_join(lkp_tab) |>
  select(-number)

row.names(landcover) <- 1:nrow(landcover)

usethis::use_data(landcover, overwrite = TRUE)
