# Title: Make Map of eDNA proposed sampling sites V2
#
# Date: 2024-06-20
#
# Author(s): Chris Madsen (chris.madsen@gov.bc.ca)
#
# Description: This script is an update to V1 of the same name. Trevor and Heather
# have sent me a more up-to-date excel file.

library(tidyverse)
library(sf)
library(openxlsx)
library(bcdata)
library(basemaps)

# Make simple BC boundary shape
# bc = bcmaps::bc_bound() |> dplyr::summarise() |> rmapshaper::ms_simplify()

# Load in data
ss = read.xlsx('data/2024 Sampling Sites_DRAFT1.xlsx', sheet = 'Mapping_locations') |>
  tidyr::as_tibble() |>
  purrr::set_names(snakecase::to_snake_case)

View(ss)

# Remove all rows including and below "OLD"

delete_row = which(ss$region == 'OLD')

ss = ss |>
  dplyr::slice(1:(delete_row-1))

# Spatialize the sampling site table; reproject to BC Albers.
ss = ss |>
  st_as_sf(coords = c("easting","northing"), crs = 2955) |>
  sf::st_transform(crs = 3005)

ggplot() +
  geom_sf(data = bcmaps::bc_bound()) +
  geom_sf(data = ss,
          aes(color = type))

library(leaflet)

ss_c = ss |> sf::st_transform(crs = 4326) |>
  dplyr::mutate(
  lat = round(st_coordinates(geometry)[,2],3),
  lng = round(st_coordinates(geometry)[,1],3)
  )

leaflet() |>
  addTiles() |>
  addMarkers(data = sf::st_transform(ss_c, 4326),
             label = ~paste0(location_1,"\n(",lat,",",lng,")"))

sample_bbox = st_bbox(st_buffer(st_as_sfc(sf::st_bbox(ss)), 10000))

# Get watersheds in area.
ws = bcdc_query_geodata('freshwater-atlas-watershed-groups') |>
  filter(INTERSECTS(ss)) |>
  collect()

ws_borders = ws |>
  dplyr::summarise()

ggplot() +
  geom_sf(data = ws_borders, fill = 'pink') +
  geom_sf(data = ws, fill = 'transparent', col = 'grey') +
  geom_sf(data = ss)


# Map bbox based on watershed shapes.
map_bb = st_bbox(ws_borders)

# Get basemaps
ext <- st_transform(st_as_sfc(map_bb), 3857)

streets = basemaps::basemap_terra(ext,map_service = 'osm', map_type = 'streets') |> as("SpatRaster")
topographic = basemaps::basemap_terra(ext,map_service = 'osm', map_type = 'topographic') |> as("SpatRaster")

terra::plot(streets)
# terra::plot(topographic)

streets = terra::project(streets, terra::crs(ss))
# topographic = terra::project(topographic, terra::crs(ss))

# Streets version
ggplot() +
  tidyterra::geom_spatraster_rgb(data = streets) +
  geom_sf(data = ws, aes(fill = WATERSHED_GROUP_NAME),
          col = 'grey', linewidth = 0.5, alpha = 0.35) +
  geom_sf(data = ws_borders, fill = 'transparent',
          col = 'black', linewidth = 0.5) +
  geom_sf(data = ss, aes(shape = type), size = 2.5) +
  coord_sf(xlim = map_bb[c(1,3)],
           ylim = map_bb[c(2,4)]) +
  labs(fill = 'Watershed Name') +
  ggthemes::theme_map() +
  theme(legend.justification.inside = c(-6,-0.6)) +
  ggspatial::annotation_scale()

ggsave(
  filename = 'output/proposed_sampling_sites_2024/proposed_sampling_sites_V2_streets_basemap.jpg',
  width = 10,
  height = 10
)

# Topographic Version
ggplot() +
  tidyterra::geom_spatraster_rgb(data = topographic,
                                 alpha = 0.5) +
  geom_sf(data = ws, aes(fill = WATERSHED_GROUP_NAME),
          col = 'grey', linewidth = 0.5, alpha = 0.35) +
  geom_sf(data = ws_borders, fill = 'transparent',
          col = 'black', linewidth = 0.5) +
  geom_sf(data = ss) +
  coord_sf(xlim = map_bb[c(1,3)],
           ylim = map_bb[c(2,4)]) +
  labs(fill = 'Watershed Name') +
  ggthemes::theme_map() +
  theme(legend.justification.inside = c(-6.5,-3.75)) +
  ggspatial::annotation_scale()

ggsave(
  filename = 'output/proposed_sampling_sites_2024/proposed_sampling_sites_topographic_basemap.jpg',
  width = 10,
  height = 10
)
