library(readxl)
library(tidyverse)
library(sf)
library(bcdata)
library(bcinvadeR)

# List of species by scientific name and common name.
sp_list = data.frame(
  eng_name = c("cutthroat trout","coho salmon","rainbow trout","sockeye salmon","chinook salmon","mountain whitefish",
               "Atlantic salmon","brown trout","bull trout","brook trout"),
  sci_name = c("Oncorhynchus clarkii","Oncorhynchus kisutch","Oncorhynchus mykiss","Oncorhynchus nerka",
               "Oncorhynchus tshawytscha","Prosopium williamsoni","Salmo salar","Salmo trutta",
               "Salvelinus confluentus","Salvelinus fontinalis")
)

# Read in the DFO SARA-listed species
dfo = sf::read_sf("../../long_term_projects/DFO_SARA/output/dfo_sara_occurrences_in_BC_all_species.gpkg")

dfo_sp = dfo |>
  dplyr::filter(dfo$Scientific_Name %in% sp_list$sci_name)
rm(dfo)

# Read in our provincial SAR layer
sar = bcdc_query_geodata('species-and-ecosystems-at-risk-publicly-available-occurrences-cdc') |>
  bcdata::filter(SCI_NAME %in% c(sp_list$sci_name)) |>
  collect()
# No results.

public_recs = sp_list$eng_name |>
  purrr::map(~ {
    suppressMessages(bcinvadeR::grab_aq_occ_data(.x))
  }, .progress = T)

public_recs_b = dplyr::bind_rows(public_recs)

public_recs_b = dplyr::rename(public_recs_b, geom = geometry)

# ggplot() + geom_sf(data = public_recs_b)

# Combine data sources.
dfo_merge = dfo_sp |>
  dplyr::select(Species = Common_Name_EN,
                Location = Waterbody,
                Population = Population_EN,
                SARA_Status) |>
  sf::st_transform(4326)

all_recs = public_recs_b |>
  dplyr::bind_rows(dfo_merge)

saveRDS(all_recs, "data/whirling_disease_susceptible_species_occ_data.rds")
