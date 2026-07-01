library(sf)
library(flyer)

out <- 'data_dl/geojson'
dir.create(out, showWarnings = FALSE, recursive = TRUE)

datasets <- list(
  places      = places,
  route       = route,
  land        = land,
  topography  = topography,
  bathymetry  = bathymetry,
  landcover   = landcover,
  protected   = protected
)

for (nm in names(datasets)) {
  f <- file.path(out, paste0(nm, '.geojson'))
  st_write(datasets[[nm]], f, driver = 'GeoJSON', delete_dsn = TRUE, quiet = TRUE)
}

write.csv(
  data.frame(class = names(lc_pal), color = unname(lc_pal)),
  file.path(out, 'landcover_palette.csv'),
  row.names = FALSE
)

writeLines(
  c(
    'flyer geospatial data — GeoJSON edition',
    '',
    'One GeoJSON file per data set, mirroring the GPKG bundle in flyer-geodata.zip.',
    'landcover_palette.csv provides the class -> hex color mapping used with landcover.',
    '',
    'CRS: EPSG:4326 (WGS 84) for all layers.',
    'See the flyer R package for full documentation: https://sagesteppe.github.io/flyer'
  ),
  file.path(out, 'README.txt')
)
