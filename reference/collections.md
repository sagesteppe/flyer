# Species lists for collection sites

Species lists for collections sites from Brusco 2020 ('the manuscipt')

## Usage

``` r
collections
```

## Format

an sf tibble with one row per collection

- scientific_name:

  Name of the collection per the manuscript

- type_specimen:

  Marked as a type based on the manuscript

- synonyms:

  Synonym names mentioned in the manuscript

- other_notes:

  notes on the collection from the manuscript

- questionable_id:

  notes from the manuscript on identification status

- site:

  Collection site name per the manuscript

- gbif_usage_key:

  GBIF usage key for integration with GBIF

- gbif_scientific_name:

  GBIF used scientific name for the collection

- gbif_match_type:

  How the 'scientific_name' was resolved with GBIF

- gbif_status:

  Whether GBIF recognizes a particular name applied to the collection

- gbif_url:

  Hyperlink to the GBIF page for the resolved entry.

- collection_site:

  Collection site name per the manuscript

- location_english:

  flyer package name for the site from 'places'

## Examples

``` r
data(collections)
head(collections)
#> # A tibble: 6 × 13
#>   type_specimen synonyms repository other_notes  questionable_id scientific_name
#>   <chr>         <chr>    <chr>      <chr>        <chr>           <chr>          
#> 1 NA            NA       NA         Hawksbill t… FALSE           Eretmochelys i…
#> 2 NA            NA       NA         symbiotic h… FALSE           Obelia dichoto…
#> 3 NA            NA       NA         pelagic cra… FALSE           Planes minutus 
#> 4 NA            NA       NA         the pelagic… FALSE           Pleuroncodes p…
#> 5 NA            NA       NA         the most co… FALSE           Geodia mesotri…
#> 6 NA            NA       NA         one of the … FALSE           Aglaophenia di…
#> # ℹ 7 more variables: gbif_usage_key <chr>, gbif_scientific_name <chr>,
#> #   gbif_match_type <chr>, gbif_status <chr>, gbif_url <chr>,
#> #   collection_site <chr>, location_english <chr>

## if you want to make it spatial
data(places)
collections_sf <- dplyr::left_join(
     collections, 
     dplyr::select(places, location_english, geometry),
     by = 'location_english'
) |>
  sf::st_as_sf()
```
