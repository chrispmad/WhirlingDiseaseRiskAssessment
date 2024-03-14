# Title: Get Water Chemistry Data for Whirling Disease Analysis
#
# Date: 2024-03-06
#
# Author(s): Chris Madsen (chris.madsen@gov.bc.ca)
#
# Description: This script ingests the Environmental Monitoring System database for
# water chemistry parameters.

# We download the EMS database periodically for the Invasive Mussel Defence Program
# since it informs the calcium mapping exercise that underpins much of the
# habitat suitability estimation for Zebra-Quagga Mussels in B.C.

# Therefore, we pull from the same files for this analysis.

ems_filepaths = list.files('../../long_term_projects/ZQMussels/01_DataCleaning/data/',
           pattern = 'ems_sample.*',
           full.names = T)

# When were these files downloaded?
ems_download_datetime = file.mtime(ems_filepaths[1])
ems_download_datetime

# If this is considered too long ago, consider redownloading the EMS data files
# from the BC Data Catalogue.

# Let's check out what variables are in the EMS dataset by reading in the
# smaller, recent data portion.
rec_dat_filepath = ems_filepaths[stringr::str_detect(ems_filepaths,'current')]
historic_dat_filepath = ems_filepaths[stringr::str_detect(ems_filepaths,'hist')]

ems_recent = vroom::vroom(rec_dat_filepath)

names(ems_recent)

# Let's explore the different parameters.

ems_params = unique(ems_recent$PARAMETER)

# 564 parameters... wowza. Let's just look for inorganic and organic carbon.
ems_params_carbon = ems_params[stringr::str_detect(ems_params, '[C,c]arbon')]

ems_params_carbon_org = ems_params_carbon[stringr::str_detect(ems_params_carbon, ' [o,O]rganic')]
ems_params_carbon_inorg = ems_params_carbon[stringr::str_detect(ems_params_carbon, ' [i,I]norganic')]

# Pull these parameters out from the recent and historic EMS data chunks.
ems_recent_c = ems_recent |>
  dplyr::filter(PARAMETER %in% c(ems_params_carbon_org,ems_params_carbon_inorg))

# Define the function we'll use to filter each chunk as we read progressively
# through this massive .csv file.
callback_function <- function(x, pos) subset(x, PARAMETER %in% c(ems_params_carbon_org,ems_params_carbon_inorg))

library(readr)

ems_historic_c = readr::read_csv_chunked(
  file = historic_dat_filepath,
  callback = DataFrameCallback$new(callback_function)
)

ems_all_c = dplyr::bind_rows(
  ems_recent_c,
  ems_historic_c
)

write.csv(ems_all_c, 'data/ems_all_carbon_records.csv')
