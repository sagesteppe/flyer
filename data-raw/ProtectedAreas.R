# protected areas
setwd('~/Documents/flyer/data-raw')

library(sf)
library(tidyverse)

p <- './protected_areas'

bb <- st_as_sfc(st_bbox(c(xmin = -121, xmax = -103, ymin = 19, ymax = 35.3),  crs = st_crs(4326)))
mx <- st_read( file.path(p, 'anpmx/anpmx.shp')) |>
  mutate(reserve.type = case_when(
    S_TERRES > 0 & S_MARINA == 0 ~ 'Terrestrial',
    S_TERRES == 0 & S_MARINA > 0 ~ 'Marine',
    S_TERRES > S_MARINA ~ 'Primarily Terrestrial',
    S_TERRES < S_MARINA ~ 'Primarily Marine',
    )
  ) |>
  select(name = NOMBRE, date.established = PRIM_DEC, reserve.type) |>
  st_make_valid()

mx <- st_crop(mx, bb)

ggplot() +
  geom_sf(data = mx)

format(object.size(mx), units = 'MB')

mx <- rmapshaper::ms_simplify(mx, keep = 0.05)
st_write(mx, 'mx.gpkg', append = F)
format(object.size(mx), units = 'MB')

protected <- mx

usethis::use_data(protected)

rm(mx, bb, p, protected)
