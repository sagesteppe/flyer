# Sync dataset.jsonld into _pkgdown.yml so the JSON-LD ships in the site head.
# Run whenever dataset.jsonld changes: Rscript data-raw/sync_jsonld.R
suppressPackageStartupMessages(library(yaml))

jsonld <- paste(readLines('dataset.jsonld'), collapse = '\n')

cfg <- read_yaml('_pkgdown.yml')

cfg$template$includes <- list(
  in_header = paste0(
    '<script type="application/ld+json">\n',
    jsonld,
    '\n</script>'
  )
)

write_yaml(cfg, '_pkgdown.yml')
cat('synced dataset.jsonld -> _pkgdown.yml (template.includes.in_header)\n')
