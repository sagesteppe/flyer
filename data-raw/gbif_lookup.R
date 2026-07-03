# gbif_lookup.R
# Bind all Site*.csv species lists, clean scientific names, and match each
# to a GBIF taxon so a user can click straight through to the species page
# (https://www.gbif.org/species/<usageKey>).
#

library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(rgbif)

# 1. Bind every Site*.csv in the working directory, tagging the source file
getwd()
p2d <- file.path('data-raw', 'species')
site_files <- file.path(p2d, list.files(p2d, pattern = "^Site\\d+\\.csv$"))

species_all <- map_dfr(site_files, function(f) {
  read_csv(f, col_types = cols(.default = "c")) |>
    rename_with(str_to_title) |>            # normalize header case (Site4.csv is lowercase)
    mutate(Site = str_remove(f, "\\.csv$"))
})

# 2. Clean scientific names: drop leading *, **, ?, quote marks, and any
#    trailing "?" or bracketed uncertainty markers, then trim whitespace
species_all <- species_all |>
  mutate(
    Clean_Name = `Scientific Name` |>
      str_remove_all('[“”"]') |>
      str_remove('^[\\*\\?\\s,]+') |>
      str_remove('\\?+$') |>
      str_squish()
  )

# 3. Look up each unique cleaned name against the GBIF backbone
`%||%` <- function(a, b) if (length(a)) a else b
unique_names <- unique(species_all$Clean_Name)

gbif_matches <- map_dfr(unique_names, function(nm) {
  hit <- tryCatch(name_backbone(name = nm, verbose = FALSE), error = function(e) NULL)
  tibble(
    Clean_Name = nm,
    gbif_usageKey = hit$usageKey %||% NA_character_,
    gbif_scientificName = hit$scientificName %||% NA_character_,
    gbif_matchType = hit$matchType %||% NA_character_,
    gbif_status = hit$status %||% NA_character_
  )
})

# 4. Join matches back on, build the clickable GBIF species page URL
species_all1 <- species_all |>
  left_join(gbif_matches, by = "Clean_Name") |>
  mutate(
    gbif_url = if_else(!is.na(gbif_usageKey),
                        paste0("https://www.gbif.org/species/", gbif_usageKey),
                        NA_character_)
  )

##
name_links = read.csv(file.path('data-raw', 'species', 'collection_summary.csv')) |>
  janitor::clean_names() |>
  select(collection_no, collection_site = location, location_english = simple_name) |>
  mutate(collection_no = as.character(collection_no))

species_all = species_all |>
  janitor::clean_names() |>
  mutate(site = str_extract(site, '\\d.*')) |>
  left_join(name_links, by = c('site' = 'collection_no')) |>
  select(-site, -scientific_name, scientific_name = clean_name) 

# 5. Write the combined, GBIF-linked table
collections = species_all

usethis::use_data(species_all, overwrite = TRUE)
write_csv(collections, file.path('data_dl', 'collections.csv'))
