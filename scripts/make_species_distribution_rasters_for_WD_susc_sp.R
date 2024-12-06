fras = sf::read_sf("W:/CMadsen/shared_data_sets/Fraser_River_Big_Watershed.shp")
columbia = sf::read_sf("W:/CMadsen/shared_data_sets/Columbia_River_Big_Watershed.shp")
fras_col = dplyr::bind_rows(fras, columbia) |> dplyr::summarise() |>
  sf::st_transform(4326)

wd_sus = readRDS("data/whirling_disease_susceptible_species_occ_data.rds")

wd_sus_fras_col = sf::st_intersection(wd_sus, fras_col)

# Reference raster
tub_prob_w_carbon = terra::rast('output/Predicted_Ttubifex_density.tif') |>
  terra::project("EPSG:4326")

# Split polygons and points
wd_points = wd_sus_fras_col |> dplyr::filter(sf::st_geometry_type(geom) == "POINT")
wd_polys = wd_sus_fras_col |> dplyr::filter(sf::st_geometry_type(geom) %in% c("MULTIPOLYGON","POLYGON"))

wd_list = list(wd_polys,wd_points)

# Rasterize each species presence
wd_sp_dist_rasts_l = wd_list |>
  purrr::map( ~ {
    .x |>
      dplyr::group_by(Species) |>
      dplyr::group_split() |>
      purrr::map( ~ {
        the_rast = terra::rasterize(.x, tub_prob_w_carbon)
        names(the_rast) = snakecase::to_snake_case(unique(.x$Species))
        the_rast
      })
  }, .progress = T)

wd_sp_dist_rasts = wd_sp_dist_rasts_l |>
  list_flatten() |>
  sprc()

saveRDS(wd_sp_dist_rasts,"output/whirling_disease_susceptible_species_distribution_rasters.rds")

