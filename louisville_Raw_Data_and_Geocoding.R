library(readr)
library(stringi)
library(dplyr)


#----------------------------------------------------------------------------------
# Raw Data loaded and converted to lower case for readability
# Dates converted to POSIXct format for date manipulation
# Whitespace trimmed
raw_data <- read_csv("D:/RProgram/LouisvilleCrime/cityComparison/Crime_Data_All_Louisville.csv")
names(raw_data) <- tolower(names(raw_data))
raw_data <- data.frame(lapply(raw_data, tolower), stringsAsFactors = FALSE)
raw_data <- tbl_df(raw_data)
raw_data$date_occured <- as.POSIXct(raw_data$date_occured, format = "%Y-%m-%d %H:%M:%S")
raw_data$date_reported <- as.POSIXct(raw_data$date_reported, format = "%Y-%m-%d %H:%M:%S")
raw_data$uor_desc <- stri_trim_both(raw_data$uor_desc, pattern = "\\P{Wspace}")
raw_data$block_address <- stri_trim_both(raw_data$block_address, pattern = "\\P{Wspace}")
str(raw_data)


# ----------------------------------------------------------------------------------
# Filtering raw data to remove characters that will trip up the geocoding. Then
# concatenating city and zip code with block address to form a full address for the
# geocoding service.  Incidents without dates are removed.
crime_lou <- raw_data %>%
  filter(!is.na(date_occured), !is.na(date_reported))

crime_lou <- crime_lou %>%
  mutate(geo_add = gsub("block |/.+", "", block_address))

crime_lou <- crime_lou %>%
  mutate(full_address = paste(geo_add, "Louisville, KY", zip_code, sep = ", "))

# Loading old geocoding results to use to cut down on new geocoding needed.
# Then I select out just the lat/lng and the full address to use to join back with
# the full data set
geocoded <- readRDS("louCrime-app/Data/lou_shiny_data.rds")
just_coords <- geocoded %>%
  select(full_address, lat, lng)%>%
  distinct(full_address)

# Filter out the missing values to perform new geocoding
missing_coords <- crime_lou %>%
  filter(is.na(lat))%>%
  distinct(full_address)

# The google geocoding service is most robust, but also has a 2500/day limit
# I tried a couple alternatives, but eventually decided to batch it into days
# I geocode 2450 a day and then merge the results into the full dataset.
# Below I load the temporary geocoding file and merge it into the full dataset
geocoded_small1 <- readRDS("input_temp_geocoded.rds")
geocoded_small2 <- readRDS("input_temp_geocoded2.rds")
geocoded_small3 <- readRDS("input_temp_geocoded3.rds")
geocoded_small4 <- readRDS("input_temp_geocoded4.rds")
geocoded_small5 <- readRDS("input_temp_geocoded5.rds")

geocoded_small_merged <- rbind(geocoded_small1, geocoded_small2, geocoded_small3,
                               geocoded_small4, geocoded_small5)%>%
  select(supplied_address, lat, lng)
names(geocoded_small_merged)[names(geocoded_small_merged) == "supplied_address"] <- "full_address"

just_coords <- rbind(just_coords, geocoded_small_merged)
crime_lou <- left_join(crime_lou, just_coords, by = "full_address")

# For some reason, this merge results in duplicate rows.  Filter to get rid of them
# until I figure out the reason behind it.
crime_lou <- crime_lou %>%
  distinct(id)

#----------------------------------------------------------------------------------


#----------------------------------------------------------------------------------
# Write this data set out to be used in other scripts
write.csv(crime_lou, file = "crime_lou_with_geocoding.csv", row.names = FALSE)
