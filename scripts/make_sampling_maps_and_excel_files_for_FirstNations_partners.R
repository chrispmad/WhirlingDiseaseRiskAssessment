
#libraries
library(crosstalk)
library(tidyverse)
library(sf)
library(openxlsx)
library(bcdata)
library(basemaps)
library(DT)
library(ggspatial)
library(leaflet)


setwd(here::here())

# Load in data
ss = read.xlsx('data/2024 Sampling Sites_DRAFT1.xlsx', sheet = 'Mapping_locations') |>
  tidyr::as_tibble() |>
  purrr::set_names(snakecase::to_snake_case)

# delete_row = which(ss$region == 'OLD')
#
# ss = ss |>
#   dplyr::slice(1:(delete_row-1))

# Spatialize the sampling site table; reproject to BC Albers.
ss = ss |>
  st_as_sf(coords = c("easting","northing"), crs = 2955) |>
  sf::st_transform(crs = 3005)

sample_bbox = st_bbox(st_buffer(st_as_sfc(sf::st_bbox(ss)), 10000))

if(!file.exists('data/interactive_sampling_V2_data/ws.gpkg')){

  # Get watersheds in area.
  ws = bcdc_query_geodata('freshwater-atlas-watershed-groups') |>
    filter(INTERSECTS(ss)) |>
    collect()

  sf::write_sf(ws, 'data/interactive_sampling_V2_data/ws.gpkg')
} else {
  ws = sf::read_sf('data/interactive_sampling_V2_data/ws.gpkg')
}

ws_borders = ws |>
  dplyr::summarise()

ws_borders_s = rmapshaper::ms_simplify(ws_borders)

if(!file.exists('data/interactive_sampling_V2_data/big_rivers_in_ws.gpkg')){
  # Nab large rivers in that area.
  big_rivers_in_ws = bcdc_query_geodata('freshwater-atlas-stream-network') |>
    bcdata::filter(STREAM_ORDER > 5) |>
    bcdata::filter(INTERSECTS(ws_borders_s)) |>
    collect() |>
    sf::st_zm()

  big_rivers_in_ws = big_rivers_in_ws |>
    dplyr::group_by(GNIS_NAME, BLUE_LINE_KEY, STREAM_ORDER) |>
    dplyr::summarise() |>
    dplyr::ungroup()

  big_rivers_in_ws = sf::st_transform(big_rivers_in_ws, 4326)

  sf::write_sf(big_rivers_in_ws, 'data/interactive_sampling_V2_data/big_rivers_in_ws.gpkg')
} else {
  big_rivers_in_ws = sf::read_sf('data/interactive_sampling_V2_data/big_rivers_in_ws.gpkg')
}

# Map bbox based on watershed shapes.
map_bb = st_bbox(ws_borders)

ss = sf::st_transform(ss, 4326)

# Separate Parks Canada points from ss
# pc = ss |> dplyr::filter(priority == 'Park Canada')

# ss = ss |> dplyr::filter(priority != 'Park Canada')

# Make crosstalk object for ss
# ss_shared = SharedData$new(ss)

ws_colour = leaflet::colorFactor(palette = 'Dark2',
                                 domain = ws$WATERSHED_GROUP_NAME)

ss_method = leaflet::colorFactor(palette = 'Spectral',
                                 domain = ss$method)

# subsets of ss
ws_1 = ws |>
  dplyr::filter(WATERSHED_GROUP_NAME %in% c('Adams River','Revelstoke Lake','Upper Arrow Lake',
                                            'Lower Arrow Lake','Kettle River','Okanagan River')) |>
  sf::st_transform(4326)

ss_1 = ss |>
  sf::st_filter(ws_1)

ws_2 = ws |>
  dplyr::filter(WATERSHED_GROUP_NAME %in% c('Kicking Horse River','Kootenay River','Columbia River')) |>
  sf::st_transform(4326)

ss_2 = ss |>
  sf::st_filter(ws_2)

ws_3 = ws |>
  dplyr::filter(WATERSHED_GROUP_NAME %in% c('Kootenay Lake','Bull River','Elk River',
                                            'St. Mary River')) |>
  sf::st_transform(4326)

ss_3 = ss |>
  sf::st_filter(ws_3) |>
  dplyr::bind_rows(ss |> dplyr::filter(location_1 == 'Whiteswan'))

# Make excel files
openxlsx::write.xlsx(ss_1 |> sf::st_drop_geometry(), "output/proposed_sampling_sites_2024/excel_files/proposed_sample_sites_orange.xlsx")
openxlsx::write.xlsx(ss_2 |> sf::st_drop_geometry(), "output/proposed_sampling_sites_2024/excel_files/proposed_sample_sites_red.xlsx")
openxlsx::write.xlsx(ss_3 |> sf::st_drop_geometry(), "output/proposed_sampling_sites_2024/excel_files/proposed_sample_sites_yellow.xlsx")

# Make maps

# 1. Yellow
m = leaflet(elementId = 'map',
        height = 800, width = 1000
        ) |>
  addTiles(group = 'Streets') |>
  # addProviderTiles(provider = providers$OpenTopoMap,
  #                  options = providerTileOptions(opacity = 0.5),
  #                  group = 'Topography') |>
  addLayersControl(
    position = 'bottomright',
    baseGroups = c('Streets','Topography'),
    overlayGroups = c('watersheds','large rivers','sample sites'),
    options = layersControlOptions(collapsed = F)
  ) |>
  addScaleBar('bottomleft') |>
  leaflet.extras::addResetMapButton() |>
  addPolygons(
    weight = 1,
    data = st_transform(ws, 4326),
    label = ~paste0("Watershed: ",WATERSHED_GROUP_NAME),
    color = ~ws_colour(WATERSHED_GROUP_NAME),
    fillColor = ~ws_colour(WATERSHED_GROUP_NAME),
    fillOpacity = 0.20,
    group = 'watersheds'
  ) |>
  addPolylines(
    data = big_rivers_in_ws,
    label = ~paste0("River: ",GNIS_NAME),
    weight = ~(STREAM_ORDER/5)^2,
    color = 'darkblue',
    opacity = 0.75,
    group = 'large rivers'
  ) |>
  addCircleMarkers(
    data = ss,
    label = ~paste0("Sampling Location: ",location_1," (",location_2,")"),
    radius = 5,
    weight = 1,
    color = 'black',
    fillColor = ~ss_method(method),
    opacity = 1,
    fillOpacity = 0.8,
    group = 'sample sites'
  ) |>
addLegend(title = "Watersheds",
          position = 'topright',
          pal = ws_colour,
          values = ws$WATERSHED_GROUP_NAME) |>
  addLegend(title = "Sample Method",
            position = 'bottomleft',
            pal = ss_method,
            values = ss$method)

m_1 = m |>
  addCircleMarkers(
    data = ss_1,
    color = 'gold',
    fillColor = 'gold',
    radius = 8
  )

m_2 = m |>
  addCircleMarkers(
    data = ss_2,
    color = 'gold',
    fillColor = 'gold',
    radius = 8
  )

m_3 = m |>
  addCircleMarkers(
    data = ss_3,
    color = 'gold',
    fillColor = 'gold',
    radius = 8
  )


library(mapview)

mapshot(m_1, file = "output/proposed_sampling_sites_2024/proposed_sampling_map_orange.jpg")
mapshot(m_2, file = "output/proposed_sampling_sites_2024/proposed_sampling_map_red.jpg")
mapshot(m_3, file = "output/proposed_sampling_sites_2024/proposed_sampling_map_yellow.jpg")

