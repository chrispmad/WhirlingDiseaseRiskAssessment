# Title: Test Tubifex prediction accuracy with known occurrences of whirling disease
#
# Date: 2024-xx-xx
#
# Author(s): Chris Madsen (chris.madsen@gov.bc.ca)
#
# Description: This script ...

library(tidyverse)
library(terra)
library(sf)
library(bcdata)

tub = terra::rast('output/Predicted_Ttubifex_density.tif')

# Preliminary results found suspected cases of whirling disease in the Kicking Horse River,
# Wapta Lake, Finn Creek, Monarch Creek and the confluence of Emerald River and the
# Kicking Horse River.

# Download waterbodies...
streams = bcdc_query_geodata('freshwater-atlas-stream-network') |>
  filter(GNIS_NAME %in% c('Kicking Horse River','Wapta Lake',
                          'Monarch Creek','Emerald River')) |>
  collect() |>
  # Remove height dimension... it wonks up a bunch of spatial functions...
  sf::st_zm() |>
  # Merge stream pieces by watershed group ID and stream name
  dplyr::group_by(WATERSHED_GROUP_ID,GNIS_NAME) |>
  dplyr::summarise() |>
  # Reproject the geometries of these guys into WGS 84 EPSG 4326 (i.e. lat and long)
  sf::st_transform(crs = 4326) |>
  dplyr::ungroup()

lakes = bcdc_query_geodata('freshwater-atlas-lakes') |>
  filter(GNIS_NAME_1 %in% c('Wapta Lake')) |>
  collect() |>
  dplyr::rename(GNIS_NAME = GNIS_NAME_1) |>
  dplyr::group_by(GNIS_NAME) |>
  dplyr::summarise() |>
  sf::st_transform(crs = 4326) |>
  dplyr::ungroup()

wbs = dplyr::bind_rows(
  streams,
  lakes
)

my_bbox = sf::st_bbox(wbs)

bc = bcmaps::bc_bound() |>
  sf::st_transform(4326)

# Take a quick gander at these wbs overlaid on the prediction raster
tub_4326 = as.data.frame(tub, xy = TRUE) |>
  sf::st_as_sf(coords = c('x','y'), crs = 3005) |>
  sf::st_transform(4326) |>
  sf::st_crop(wbs) |>
  dplyr::mutate(lat = st_coordinates(geometry)[,2],
                lon = st_coordinates(geometry)[,1]) |>
  sf::st_drop_geometry() |>
  as_tibble()

tub_df = as.data.frame(terra::project(tub,terra::crs(vect(bc))), xy = TRUE)

ggplot() +
  geom_sf(data = bc) +
  geom_raster(data=tub_df, aes(x=x,y=y,fill = predicted_tubifex_density)) +
  # geom_sf_text(data = wbs[wbs$GNIS_NAME == 'Wapta Lake',], aes(label = 'Wapta Lake')) +
  geom_sf(data = wbs, aes(col = GNIS_NAME)) +
  scale_fill_gradient2(low = 'white', high = 'red', mid = 0.8) +
  coord_sf(xlim = c(my_bbox[c(1,3)]),
           ylim = c(my_bbox[c(2,4)]))
