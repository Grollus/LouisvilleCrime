# Source in the csv file
library(data.table)
library(dplyr)
library(ggmap)
library(readr)
# Set wd to LouisvilleCrime folder
#full_addresses <- fread("addresses_to_geocode.csv", data.table = FALSE)

#-----------------------------------------------------------------------------------
# Basic geocoding function
# This would timeout every so often and I had to manually restart
# A full fledged implementation of the geocoding function needs to address this
# TODO: Address timeout error in geocode function.
    # Perhaps check for the error and if present, restart the function?
  # zip_codes <-  lou_clean %>%
  #   select(zip_code) %>%
  #   distinct(zip_code)
# Geocoding full addresses now with dsk api
  # 2.52 ggmap allows use of dsk instead of google maps api

geocodeResults <- function(address){
  # query using dsk api: should be no limit
  geocode_reply <- geocode(address, output = "all", messaging = TRUE,
                           override_limit = TRUE, source = "dsk")
  
  # extracting information we want
  answer <- data.frame(supplied_address = address, lat = NA, lng = NA, 
                         returned_address = NA, status = NA)
  answer$status <- geocode_reply$status
  
  # For google API use only
  # pausing while we are over query limit
  while(geocode_reply$status == "OVER_QUERY_LMIT"){
    print("OVER QUERY LIMIT -pausing for 1 hour at:")
    print(as.character(Sys.time()))
    Sys.sleep(60*60)
    geocode_reply <- geocode(address, output = "all", messaging = TRUE,
                             override_limit = TRUE, source = "dsk")
    answer$status <- geocode_reply$status
  }
  
  # return NA's if status != "OK"
  if(geocode_reply$status != "OK"){
    return(answer)
  }
  
  # else: extract what information I am looking for
  answer$lat <- geocode_reply$results[[1]]$geometry$location$lat
  answer$lng <- geocode_reply$results[[1]]$geometry$location$lng
  answer$returned_address <- geocode_reply$results[[1]]$formatted_address
  
  return(answer)
  
}

full_addresses <- fread('missing_coords.csv', data.table = FALSE)
# initializing the data frame for the results
  geocoded <- data.frame()
  # looking for existing temp file. if it exists find where it left off and start
  # from there
  startindex <- 1
  tempfilename <- paste0("input", "_temp_geocoded.rds")
  if(file.exists(tempfilename)){
    print("Found file - resuming from index:")
    geocoded <- readRDS(tempfilename)
    startindex <- nrow(geocoded)
    print(startindex)
  }

  for(i in seq(startindex, nrow(full_addresses))){
    print(paste("Working on index", i, "of", nrow(full_addresses)))
    # query geocoder
    result <- geocodeResults(full_addresses[i,])
    print(result$status)
    result$index <- i
    # append the answer to the results file
    geocoded <- rbind(geocoded, result)
    # save temporary results as we go
    saveRDS(geocoded, tempfilename)
  }
  
# Join new geocoding to old results
temp_old <- read_csv("addresses_and_coords.csv")
new_geocoded <- geocoded%>%
  mutate(full_address = supplied_address)%>%
  select(full_address, lat, lng)

joined_results <- bind_rows(temp_old, new_geocoded)
write_csv(joined_results, path = "addresses_and_coords.csv")
  
  #----------------------------------------------------------------------------------

  
# adding lat/lng data to clean louisville data frame
full_data <- read_csv("lou_crime_without_coords.csv")
full_wcoords <- left_join(full_data, joined_results,
                            by = "full_address")

# write full data frame to csv for easy access
write_csv(full_wcoords, path = "lou_crime_geocoded_df.csv")

