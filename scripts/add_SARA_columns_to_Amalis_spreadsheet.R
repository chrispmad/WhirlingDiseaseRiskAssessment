#
library(tidyverse)
library(sf)
library(readxl)

# Excel file / coordinates
coords_xl = readxl::read_excel("J:/2 SCIENCE - Invasives/GENERAL/Research/eDNA/University of Alberta - Whirling Disease/2024 Sampling Sites_ALL_w_lat_long.xlsx")
coords_sf = sf::st_as_sf(coords_xl, coords = c("Long","Lat"), crs = 4326)

my_bbox = st_bbox(coords_sf)

# SARA - unsimplfiied geometry
sar_unsimplified = sf::read_sf("../../long_term_projects/species_at_risk_visualizer/data/DFO_SARA_occ_data_QGIS_simplified.gpkg")

ggplot() +
  geom_sf(data = sar_unsimplified)

# SARA - simplified geometry
sar = sf::read_sf("../../long_term_projects/species_at_risk_visualizer/app/www/DFO_SARA_occ_data_QGIS_very_simplified.gpkg")

# I'll have to revisit where this simplified file comes from... but it's our best shot!

ggplot() +
  geom_sf(data = sar) +
  geom_sf(data = coords_sf) +
  coord_sf(xlim = my_bbox[c(1,3)],
           ylim = my_bbox[c(2,4)])

# Buffer our coordinates by, say, 1 km and then see what overaps we have
coords_b = st_buffer(coords_sf, 2000)

ggplot() +
  geom_sf(data = sar) +
  geom_sf(data = coords_b, fill = 'purple', alpha = 0.5) +
  geom_sf(data = coords_sf) +
  coord_sf(xlim = my_bbox[c(1,3)],
           ylim = my_bbox[c(2,4)])

# coords_w_sar = coords_sf |>
#   st_join(sar)

coords_w_sar = coords_b |>
  st_join(sar_unsimplified)

coords_w_sar_sum = coords_w_sar |>
  dplyr::distinct() |>
  dplyr::group_by(Reach, `Sample Name (Site name + replicate #)`,
                  Delivery, Easting, Northing, `Nearest town or city`,
                  `Measures taken`) |>
  dplyr::reframe(`Number of SARA-listed species` = n(),
                 `SARA-listed species` = paste0(Common_Name_EN, collapse = ', ')) |>
  sf::st_drop_geometry() |>
  dplyr::mutate(`Number of SARA-listed species` = ifelse(`SARA-listed species` == 'NA', 0, `Number of SARA-listed species`))

openxlsx::write.xlsx(coords_w_sar_sum,
                     "J:/2 SCIENCE - Invasives/GENERAL/Research/eDNA/University of Alberta - Whirling Disease/2024 Sampling Sites_2km_buffer_ALL_w_SARA_overlaps.xlsx")
