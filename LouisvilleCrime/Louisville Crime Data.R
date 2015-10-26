# http://api.louisvilleky.gov/api/File/DownloadFile?fileName=Crime_Data_All.zip
unzip("Crime_Data_All.zip")
unzip("AssaultedOfficerData.zip")
#Reading data in
library(data.table)
library(dplyr)

crime_raw <- fread("Crime_Data_All.csv", stringsAsFactors = FALSE, 
                   data.table = FALSE)
assaulted_officers_raw <- fread("AssaultedOfficerData.csv", stringsAsFactors = FALSE,
                                data.table = FALSE)

#tidying up formatting
names(crime_raw) <- tolower(names(crime_raw))
crime_raw$crime_type <- tolower(crime_raw$crime_type)
crime_raw$ucr_hierarchy <- tolower(crime_raw$ucr_hierarchy)
crime_raw$att_comp <- tolower(crime_raw$att_comp)
crime_raw$lmpd_division <- tolower(crime_raw$lmpd_division)
crime_raw$premise_type <- tolower(crime_raw$premise_type)
crime_raw$block_address <- tolower(crime_raw$block_address)
crime_raw$city <- tolower(crime_raw$city)
crime_raw$uor_desc <- tolower(crime_raw$uor_desc)


#column convertions
crime_raw$date_reported <- as.POSIXct(crime_raw$date_reported, format = "%Y-%m-%d %H:%M:%S")
crime_raw$date_occured <- as.POSIXct(crime_raw$date_occured, format = "%Y-%m-%d %H:%M:%S")
crime_raw$crime_type <- as.factor(crime_raw$crime_type)
crime_raw$nibrs_code <- as.factor(crime_raw$nibrs_code)
crime_raw$ucr_hierarchy <- as.factor(crime_raw$ucr_hierarchy)
crime_raw$att_comp <- as.factor(crime_raw$att_comp)
crime_raw$lmpd_division <- as.factor(crime_raw$lmpd_division)
crime_raw$lmpd_beat <- as.factor(crime_raw$lmpd_beat)
crime_raw$premise_type <- as.factor(crime_raw$premise_type)
crime_raw$uor_desc <- as.factor(crime_raw$uor_desc)
crime_raw$block_address <- as.factor(crime_raw$block_address)
crime_raw$zip_code <- as.factor(crime_raw$zip_code)


#trimming trailing whitespace
crime_raw$block_address <- gsub("\\s+$", "", crime_raw$block_address)



#------------------------------------------------------------------------------
# Working out geocoding issue
# These are the zip_codes we are identifying as louisvile.
# Debatable list, but we will stick with it
lou_zip <- c(40056, 40118, 40201, 40202, 40203, 40204, 40205, 40206,
             40207, 40208, 40209, 40210, 40211, 40212, 40213, 40214, 
             40215, 40216, 40217, 40218, 40219, 40220, 40221, 40222, 
             40223, 40224, 40225, 40228, 40229, 40231, 40232, 40233, 
             40241, 40242, 40243, 40245, 40250, 40251, 40252, 40253,
             40255, 40256, 40257, 40258, 40259, 40261, 40266, 40268, 
             40269, 40270, 40272, 40280, 40281, 40282, 40283, 40285, 
             40287, 40289, 40290, 40291, 40292, 40293, 40294, 40295,
             40296, 40297, 40298, 40299)
lou_zip <- as.factor(lou_zip)

# List of Louisville spellings occuring in raw data set
# Will scan for these and then replace with 'louisville'
lou_city <- c("lou", "louisivlle", "louisv", "louisviille", "louisvile", "louisville",
              "lousville", "lvil")


# Cleaning up zip_code and city
crime_lou <- crime_raw %>%
  filter(zip_code %in% lou_zip == TRUE, city %in% lou_city == TRUE)


# block_address is poorly formatted for lat/lng geocoding. 'block' throws errors 
# depending on the service as does the '/' for street intersections


# How many rows contain an address with the format '/'?
sum(grepl("/", crime_lou$block_address))/nrow(crime_lou) # so 15.9% of data has 
                                                         # and address of this form

# How many contain 'block"? The rest?
sum(grepl("block", crime_lou$block_address))/ nrow(crime_lou) #78.5%

#What are the other bits?
no_block_address <- crime_lou%>%
  filter(grepl("/", crime_lou$block_address) == FALSE,
         grepl("block", crime_lou$block_address) == FALSE)
# These seem to be general 'zone' listings. Testing them out, some are geocodable,
# some are not ???





#After cleaning this would still yield ~34000 addresses to code. What if we get 
# rid of rows with 'other' nibrs codes ('000' and '999'). There is nothing immediately
# obvious I can do about those

crime_lou <- crime_raw %>%
  filter(!(nibrs_code == "000" | nibrs_code == "999"), zip_code %in% lou_zip == TRUE,
         city %in% lou_city == TRUE, !is.na(date_occured), !is.na(date_reported))



# Perhaps if we narrow the time frame we can cut down the number again
# Create a year_reported/year_occured(why the huge difference sometimes?)
# month_reported/month_occured column for easy filtering

#Filter any missing dates first
library(lubridate)
crime_lou <- crime_lou %>%
  mutate(year_occured = year(crime_lou$date_occured), 
         year_reported = year(crime_lou$date_reported),
         month_occured = month(crime_lou$date_occured, label = TRUE), 
         month_reported = month(crime_lou$date_reported, label = TRUE))

# Now if we look unique addresses
length(unique(crime_lou$block_address)) # 34024

#removing general addresses that will be difficult to geocode
to_geocode_df <- crime_lou%>%
  filter(grepl("/", crime_lou$block_address) == TRUE | grepl("block", crime_lou$block_address) == TRUE)

length(unique(to_geocode_df$block_address)) #32934

# Clean this data frame to be more interpretable to all geocoding services
to_geocode_df <- to_geocode_df%>%
  mutate(street_address = gsub("block | /.+", "", block_address))

length(unique(to_geocode_df$street_address)) #25051

# Create full address column
to_geocode_df <- to_geocode_df%>%
  mutate(full_address = paste(street_address, city, "KY", zip_code, sep = ", "))

length(unique(to_geocode_df$full_address)) #Shoots back up to 35624


# Why does this number shoot up so much? Let's take a look real quick
to_geocode_df %>%
  filter(street_address == "bishop ln")
# Aha! Our various spellings of louisvlle did us in for one
# Let's go back and fix all those to one, correct spelling
to_geocode_df <- to_geocode_df%>%
  mutate(full_address = paste(street_address, "Louisville, KY", zip_code, sep = ", "))

length(unique(to_geocode_df$full_address)) # 26072

# Filtering rows with '@#()-/.' in addresses.  Only google seems to pick those up
# during geocoding
to_geocode_df <- to_geocode_df %>%
  filter(grepl("-|#|@|/|\\.|\\(|\\)", to_geocode_df$street_address) == FALSE)

# Writing this list of unique addresses to csv for easy access and geocoding
unique_addresses <- data.frame(unique(to_geocode_df$full_address))
names(unique_addresses) <- "addresses"
write.csv(unique_addresses, "unique_addresses.csv", row.names = FALSE)

#Writing the cleaned up full data frame to csv as well
write.csv(to_geocode_df, "clean_louisville_crime.csv", row.names = FALSE)

