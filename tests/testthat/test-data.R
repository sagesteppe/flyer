expect_valid_sf <- function(x, geom_types) {
  expect_s3_class(x, "sf")
  expect_gt(nrow(x), 0)
  expect_true(sf::st_crs(x) == sf::st_crs(4326))
  expect_true(all(sf::st_geometry_type(x) %in% geom_types))
  expect_true(all(sf::st_is_valid(x)))
  expect_false(any(sf::st_is_empty(x)))
}

test_that("places loads as an sf point layer", {
  expect_valid_sf(places, "POINT")
  expect_setequal(
    names(places),
    c("location_espanol", "location_english", "collect", "real_site",
      "date_arrive", "date_depart", "geometry")
  )
})

test_that("route loads as an sf linestring layer", {
  expect_valid_sf(route, "LINESTRING")
  expect_setequal(names(route), c("destination", "geometry"))
  expect_true(all(stats::na.omit(route$destination) %in% places$location_english))
})

test_that("topography loads as an sf linestring layer", {
  expect_valid_sf(topography, "LINESTRING")
  expect_setequal(names(topography), c("elevation", "geometry"))
  expect_type(topography$elevation, "double")
})

test_that("bathymetry loads as an sf linestring layer", {
  expect_valid_sf(bathymetry, "LINESTRING")
  expect_setequal(names(bathymetry), c("elevation", "geometry"))
  expect_true(all(bathymetry$elevation <= 0))
})

test_that("protected loads as an sf polygon layer", {
  expect_valid_sf(protected, c("POLYGON", "MULTIPOLYGON"))
  expect_setequal(
    names(protected),
    c("name", "date_established", "reserve_type", "S_TERRES", "geometry")
  )
  expect_true(all(protected$reserve_type %in% c("Marine", "Terrestrial")))
})

test_that("landcover loads as an sf polygon layer", {
  expect_valid_sf(landcover, "MULTIPOLYGON")
  expect_setequal(names(landcover), c("class", "geometry"))
  expect_true(all(unique(landcover$class) %in% names(lc_pal)))
})

test_that("land loads as an sf polygon layer", {
  expect_valid_sf(land, "MULTIPOLYGON")
  expect_setequal(names(land), c("name", "geometry"))
})

test_that("lc_pal loads as a named hex color vector", {
  expect_type(lc_pal, "character")
  expect_gt(length(lc_pal), 0)
  expect_false(is.null(names(lc_pal)))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", lc_pal)))
})

test_that("collections loads as a non-spatial species table", {
  expect_s3_class(collections, "data.frame")
  expect_false(inherits(collections, "sf"))
  expect_gt(nrow(collections), 0)
  expect_true(all(
    c("scientific_name", "gbif_usage_key", "gbif_scientific_name",
      "gbif_match_type", "gbif_status", "gbif_url",
      "collection_site", "location_english") %in% names(collections)
  ))
  # every row should key onto a known place, and most names should resolve via GBIF
  expect_true(all(collections$location_english %in% places$location_english))
  expect_gt(mean(!is.na(collections$gbif_usage_key)), 0.9)
})

test_that("collections can be joined back onto places to make it spatial", {
  collections_sf <- dplyr::left_join(
    collections,
    dplyr::select(places, location_english, geometry),
    by = "location_english"
  ) |>
    sf::st_as_sf()

  expect_s3_class(collections_sf, "sf")
  expect_equal(nrow(collections_sf), nrow(collections))
  expect_false(any(sf::st_is_empty(collections_sf)))
})
