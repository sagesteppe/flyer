# Protected areas of Mexico

## Format

An sf tibble, with a handful of attributes briefly describing each
protected area.

- name:

  Official name of the protected area.

- date_established:

  Date the preserve was established.

- reserve_type:

  A locally defined category assigned to each reserve. `Marine` and
  `Terrestrial` are reserved for areas with 100\\ geometryA simplified
  multipolygon geometry.

protected A very lightly simplified data set of protected areas in
Mexico, for cartographic purposes. From CONABIO CONANP, (09/2024).
'Federal Protected Natural Areas of Mexico, September 2024', National
Commission for the Knowledge and Use of Biodiversity. Mexico City,
Mexico. data(protected)library(ggplot2) ggplot() + geom_sf(data =
protected, aes(fill = reserve_type)) + theme_minimal() datasets
