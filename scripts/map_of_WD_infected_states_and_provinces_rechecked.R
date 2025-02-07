library(tigris)
library(canadianmaps)
library(tidyverse)

# Get ALL states and provinces/territories.
usa = tigris::states(cb = TRUE)
canada = canadianmaps::PROV |> sf::st_transform(4269)

# Clip away parts of the US that are not continental / far bits of Alaska.
canada_clipper = sf::st_bbox(canada)
canada_clipper[1] <- -180
canada_clipper[2] <- 0
canada_clipper = sf::st_as_sfc(canada_clipper)

usa_c = sf::st_intersection(
  usa,
  canada_clipper
)

# Filter for just those WD-infected states / provinces / territories.

# Infected states, as of 2009, by Elwell et al.:
# Alaska, Arizona, California, Colorado, Connecticut, Idaho, Maryland,
# Massachusetts, Michigan, Montana, Nebraska, Nevada, New Hampshire,
# New Jersey, New York, Ohio, Oregon, Pennsylvania, New Mexico, Utah, Vermont,
# Virginia, Washington, West Virginia, and Wyoming

infected_states = c("AK","AZ","CA","CO","CT","ID","MD","NE","NV",
                    "NH","NJ","NM","NY","OH","OR","PA","UT","VA",
                    "WA","WV","WY","MT")

usa_wd = usa_c |>
  dplyr::mutate(orig_inf = STUSPS %in% c("WA","OR","CA","NV","ID","MT","WY","CO","UT","AZ","NM","NE",
                                         "MI","OH","WV","VA","NC","PA","NY","VT","NH","MA","RI","CT","NJ","DE","MC","DC",
                                         "AB")) |>
  dplyr::mutate(new_inf = STUSPS %in% infected_states) |>
  dplyr::select(name = STUSPS, orig_inf, new_inf)

canada_wd = canada |>
  dplyr::mutate(orig_inf = PT %in% c("WA","OR","CA","NV","ID","MT","WY","CO","UT","AZ","NM","NE",
                                     "MI","OH","WV","VA","NC","PA","NY","VT","NH","MA","RI","CT","NJ","DE","MC","DC",
                                     "AB","BC")) |>
  dplyr::mutate(new_inf = PT %in% c("AB","BC")) |>
  dplyr::select(name = PT, orig_inf, new_inf)

wd = dplyr::bind_rows(usa_wd, canada_wd) |>
  dplyr::mutate(status = 'Whirling Disease Detected') |>
  dplyr::mutate(status = ifelse(name == 'BC','First Detection in 2023',status))

ggplot() +
  geom_sf(data = usa_c) +
  geom_sf(data = canada) +
  geom_sf(data = wd[wd$orig_inf,], aes(fill = status)) +
  scale_fill_manual(values = c('Whirling Disease Detected' = 'purple')) +
  ggthemes::theme_map() +
  theme(legend.position.inside = c(0,0.25)) +
  ggspatial::annotation_scale() +
  labs(fill = 'Source Area', title = "Original Map")

p = ggplot() +
  geom_sf(data = usa_c) +
  geom_sf(data = canada) +
  geom_sf(data = wd[wd$new_inf,], aes(fill = status)) +
  scale_fill_manual(values = c('Whirling Disease Detected' = 'purple',
                               'First Detection in 2023' = 'orange')) +
  ggthemes::theme_map() +
  theme(legend.position.inside = c(0,0.25)) +
  ggspatial::annotation_scale() +
  labs(fill = 'Whirling Disease', title = "Whirling Disease Distribution (2025)",
       subtitle = "based on Elwell et al. (2009)")

p

ggsave(filename = 'output/whirling_disease_distribution_2025.jpeg',
       plot = p,
       width = 8, height = 6)
