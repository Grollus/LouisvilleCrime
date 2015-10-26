# Source in the csv file
library(data.table)
library(dplyr)
library(ggmap)
# Set wd to LouisvilleCrime folder
setwd("D:/RProgram/random_code/LouisvilleCrime")
lou_clean <- fread("clean_louisville_crime.csv", data.table = FALSE)

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
full_addresses <- lou_clean %>%
  select(full_address) %>%
  distinct(full_address)


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
#----------------------------------------------------------------------------------

  
# adding lat/lng data to clean louisville data frame
lou_clean <- left_join(lou_clean, geocoded[, 1:3],
                            by = c("full_address" = "supplied_address"))

# making zip_code factor again
lou_clean$zip_code <- as.factor(lou_clean$zip_code)

# clarify these lat/lng are for zip code, not specific street address
names(lou_clean)[names(lou_clean) == "lat"] <- "lat_zip_code"
names(lou_clean)[names(lou_clean) == "lng"] <- "lng_zip_code"

#----------------------------------------------------------------------------------
# Adding column for hour crime occured
library(lubridate)
lou_clean$hour_occured <- round(hour(lou_clean$date_occured) + minute(lou_clean$date_occured)/60, 0)
# Adding day of week column
lou_clean$weekday <- wday(lou_clean$date_occured, label = TRUE, abbr = FALSE)

# write full data frame to csv for easy access
write.csv(lou_clean, "clean_louisville_crime.csv", row.names = FALSE)

# Reorganizing offense codes for more specific browsing in app
nibrs_offenses <- data.frame(nibrs_offenses = c("Arson", "Aggravated Assault", "Simple Assault", "Intimidation",
                                                "Bribery", "Burglary/B&E", "Counterfeiting/Forgery", "Destruction/Damage/Vandalism of Property",
                                                "Drug/Narcotic Violations", "Drug/Narcotic Equip. Violations",
                                                "Embezzlement", "Extortion/Blackmail", "False Pretenses/Swindle/Confidence Games",
                                                "Credit Card/Automatic Teller Machine Fraud", "Impersonation",
                                                "Welfare Fraud", "Wire Fraud", "Betting/Wagering", "Operating/Promoting/Assisting Gambling",
                                                "Gambling Equip. Violations", "Sports Tampering", "Murder/Non-Negligent Manslaughter",
                                                "Negligent Manslaughter", "Justifiable Homicide", "Commercial Sex Acts",
                                                "Involuntary Servitude", "Kidnapping/Abduction", "Pocket Picking",
                                                "Purse Snatching", "Shoplifting", "Theft from Building", "Theft from Coin-Operated Machine or Device",
                                                "Theft from Motor Vehicle", "Theft of Motor Vehicle Parts or Accessories",
                                                "All Other Larceny"," Motor Vehicle Theft", "Pornography/Obscene Material",
                                                "Prostitution", "Assisting or Promoting Prostitution", "Purchasing Prostitution",
                                                "Robbery", "Rape", "Sodomy", "Sexual Assault with An Object", "Forcible Fondling",
                                                "Incent", "Statutory Rape", "Stolen Property Offenses", "Weapon Law Violations", 
                                                "Bad Checks", "Curfew/Loitering/Vagrancy Violations", "Disorderly Conduct",
                                                "Driving Under the Influence", "Drunkenness", "Family Offenses, Non-Violent",
                                                "Liquor Law Violations", "Peeping Tom", "Runaway", "Tresspassing", "All Other Offenses"),
                             nibrs_code = c("200", "13A", "13B", "13C", "510", "220", 
                                             "250", "290", "35A", "35B", "270", "210", 
                                             "26A", "26B", "26C", "26D", "26E", "39A", 
                                             "39B", "39C", "39D", "09A", "09B", "09C",
                                             "64A", "64B", "100", "23A", "23B", "23C", 
                                             "23D", "23E", "23F", "23G", "23H", "240",
                                             "370", "40A", "40B", "40C", "120", "11A",
                                             "11B", "11C", "11D", "36A", "36B", "280",
                                             "520", "90A", "90B", "90C", "90D", "90E",
                                             "90F", "90G", "90H", "90I", "90J", "90Z"))

lou_clean<- inner_join(lou_clean, nibrs_offenses, by = "nibrs_code")  
# writing partial dataframe for shiny app
lou_shiny <- lou_clean%>%
  select(date_occured, crime_type, uor_desc, nibrs_offenses, premise_type, zip_code, year_occured, month_occured,
         lat_zip_code, lng_zip_code, hour_occured, nibrs_code, weekday, lat, lng, full_address)

write.csv(lou_shiny, "lou_shiny_data.csv", row.names = FALSE)
