library(rgbif)
library(flyer)
library(purrr)
library(sf)
library(tidyverse)


wkt_bbox <- places |>
  filter(collect == TRUE) |>
  st_transform(32612) |>
  st_buffer(7500) |>
  st_transform(4326) |>
  mutate(
    wkt = purrr::map_chr(geometry, \(g) {
      g |>
        st_bbox() |>
        st_as_sfc() |>
        st_as_text()
    })
  ) |>
  sf::st_drop_geometry() |>
  select(location_english, date_arrive, date_depart, wkt) |>
  pivot_longer(cols = starts_with('date'), values_to = 'eventDate') |>
  select(-name) |>
  distinct()


identify_collections <- function(x) {
  rgbif::occ_search(
    year = format(as.Date(x$eventDate), '%Y'),
    month = format(as.Date(x$eventDate), '%m'),
    geometry  = x$wkt,
    basisOfRecord = 'PRESERVED_SPECIMEN'
  )
}
spl <- split(wkt_bbox, 1:nrow(wkt_bbox))
out <- lapply(spl, identify_collections)
names(out) <- wkt_bbox$location_english


all_found_collections <- dplyr::bind_rows(
  purrr::map(out, \(x) x$data),
  .id = 'location'
)

colnames(all_found_collections)

cols2select <- 
    c(
  "location", "eventDate", "scientificName", "acceptedScientificName",
  "phylum", "order", "family", "genus", "species", "taxonRank", "taxonomicStatus", "acceptedTaxonKey",
  "dateIdentified", "identifier", "recordedBy", "preparations",
  "institutionCode", "catalogNumber",  "key", "gbifID",
  'locality', 'eventRemarks'
  )

all_found_collections <- select(all_found_collections, all_of(cols2select)) |>
  filter(str_detect(recordedBy, 'Ricketts')) |>
  mutate(recordedBy = 'Edward Flanders Ricketts, John Steinbeck')

## check these sites really quick
#all_found_collections |>
#  sf::st_as_sf(coords = c(x = 'decimalLongitude', y = 'decimalLatitude'), crs = 4326) |>
#  st_write('collections_temp.gpkg')


af1 <- all_found_collections |>
  mutate(
    locality = str_remove(locality, 'Mexico, Gulf of California, |Gulf of California, '),
    locality = str_remove(locality, ', Baja California, Mexico$'),
    locality = str_replace(locality, '"Angeles Bay"', "Angeles Bay"),
    locality = str_replace(locality, "CAPE SAN LUCAS", "Cape San Lucas")
  )
