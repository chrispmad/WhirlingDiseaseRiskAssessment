library(tidyverse)
library(sf)

col = sf::read_sf('W:/CMadsen/shared_data_sets/Columbia_River_Big_Watershed.gpkg') |>
  dplyr::summarise(area = 'Columbia River Watershed')

bc = bcmaps::bc_bound() |>
  dplyr::summarise(area = 'BC') |>
  dplyr::rename(geom = geometry)

dat = dplyr::bind_rows(bc,col)

cities = bcmaps::bc_cities()

# cities = cities |> dplyr::filter(POP_2000 > 100000)
cities = cities |> dplyr::filter(NAME %in% c('Victoria','Vancouver',
                                             'Kamloops','Kelowna','Williams Lake',
                                             'Prince George','Atlin','Golden','Cranbrook',
                                             'Fort Nelson','Terrace'))

ggplot() +
  geom_sf(data = dat, aes(fill = area)) +
  scale_fill_manual(values = c('BC' = 'white', 'Columbia River Watershed' = 'grey')) +
  # geom_sf(data = cities) +
  geom_sf_text(data = cities, aes(label = NAME), col='#504845', size = 2.5) +
  ggthemes::theme_map() +
  labs(fill = 'Area')

ggsave(filename = 'output/Simple_map_of_Columbia_River_Watershed_in_BC.jpeg',
       width = 6, height = 6)
