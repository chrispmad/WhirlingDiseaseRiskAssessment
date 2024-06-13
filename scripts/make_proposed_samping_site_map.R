# Title: Make Map of proposed sampling sites
#
# Date: 2024-06-05
#
# Author(s): Chris Madsen (chris.madsen@gov.bc.ca)
#
# Description: This script takes some data that Heather Lampson
# sent me to make a map of proposed sampling sites, with
# watersheds as shaded baselayer.

library(tidyverse)
library(sf)
library(openxlsx)
library(bcdata)
library(basemaps)

# Make simple BC boundary shape
# bc = bcmaps::bc_bound() |> dplyr::summarise() |> rmapshaper::ms_simplify()

# Load in data
ss = read.xlsx('data/2024 Sampling Sites.xlsx', sheet = 'locations')

# Spatialize the sampling site table; reproject to BC Albers.
ss = ss |>
  st_as_sf(coords = c("easting","northing"), crs = 2955) |>
  sf::st_transform(crs = 3005)

sample_bbox = st_bbox(st_buffer(st_as_sfc(sf::st_bbox(ss)), 10000))

# Get watersheds in area.
ws = bcdc_query_geodata('freshwater-atlas-watershed-groups') |>
  filter(INTERSECTS(sample_bbox)) |>
  filter(!WATERSHED_GROUP_NAME %in% c("Upper Arrow Lake","Lower Arrow Lake",
                                     "Slocan River","Kootenay Lake",
                                     "Duncan Lake")) |>
  collect()

ws_borders = ws |>
  dplyr::summarise()

# Map bbox based on watershed shapes.
map_bb = st_bbox(ws_borders)

# Get basemaps
ext <- st_transform(st_as_sfc(map_bb), 3857)

streets = basemaps::basemap_terra(ext,map_service = 'osm', map_type = 'streets') |> as("SpatRaster")
topographic = basemaps::basemap_terra(ext,map_service = 'osm', map_type = 'topographic') |> as("SpatRaster")

terra::plot(streets)
terra::plot(topographic)

streets = terra::project(streets, terra::crs(ss))
topographic = terra::project(topographic, terra::crs(ss))

# Streets version
ggplot() +
  tidyterra::geom_spatraster_rgb(data = streets) +
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
  filename = 'output/proposed_sampling_sites_2024/proposed_sampling_sites_streets_basemap.jpg',
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
