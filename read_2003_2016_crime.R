library(readr)
library(stringi)
library(dplyr)
library(lubridate)

source("louisville_raw_data_and_geocoding.R")
# This script is primarily used to generate a full dataset from scratch. Once generated,
# the user can merely read in new data, clean and prep it for geocoding with 
# louisville_raw_data_and_geocoding.R and generate coords for the new addresses that are missing

# If you are looking for data from the current year(2017 as of writing), then you can just load
# the pre-processed version of the data using lou_crime_geocoded_df.csv

###############################################################################################
# Read in all the crime data and then prep it for geocoding
full_data <- as_tibble()

for(y in 2003:2016){
  print(paste0("Parsing crime from ", y))
  file_name <- paste0("Crime_Data_", y, ".csv")
  single_year <- data_import_and_clean(file_name, create.date.vars = TRUE, label.crimes = TRUE)
  full_data <- bind_rows(full_data, single_year)
}
full_data <- geocode_prep(full_data)

# Determine which addresses have not yet been geocoded. Export a csv file of these addresses 
# to send to the geocoder

missing_coords <- addresses_to_geocode(full_data)

# If coords are missing, Louisville_Crime_Geocoding.R must be run to get the missing 
# addresses lat/lng coords. These results are then saved and merged with the old geocoding
# results into addresses_and_coords.csv

# Now we take the full data set and join the latest geocoding results
addresses_wcoords <- read_csv("addresses_and_coords.csv")

full_data_with_coords <- left_join(full_data, addresses_wcoords, by = 'full_address')

# Write this dataset to csv file. This 
write_csv(full_data_with_coords, path = "lou_crime_geocoded_df.csv")


