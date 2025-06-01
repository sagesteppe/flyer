# protected areas
setwd('~/Documents/flyer/data-raw')

library(sf)
library(tidyverse)

p <- './protected_areas'

bb <- st_as_sfc(
  st_bbox(c(xmin = -121, xmax = -103, ymin = 19, ymax = 35.3),  crs = st_crs(4326))
  )
mx <- st_read( file.path(p, 'anpmx/anpmx.shp')) |>
  rowwise() |>
  mutate(
    S_TERRES = S_TERRES / sum(S_TERRES, S_MARINA),
    reserve.type = if_else(S_TERRES > 0.50, 'Terrestrial', 'Marine')
    ) |>
  select(name = NOMBRE, date_established = PRIM_DEC, reserve_type = reserve.type, S_TERRES) |>
  st_make_valid()

mx <- st_crop(mx, bb)

ggplot() +
  geom_sf(data = mx, aes(fill = reserve_type))

format(object.size(mx), units = 'MB')

mx <- rmapshaper::ms_simplify(mx, keep = 0.05)
st_write(mx, 'mx.gpkg', append = F)
format(object.size(mx), units = 'MB')

protected <- mx

usethis::use_data(protected, overwrite = TRUE)
st_write(protected, '../docs/protected.gpkg', append = F)

rm(mx, bb, p, protected)
