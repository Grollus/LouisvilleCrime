library(dplyr)
library(leaflet)
library(ggmap)


louCrime <- readRDS("Data/lou_shiny_data.rds")
louCrime$month_occured <- factor(louCrime$month_occured, levels = c("Jan", "Feb", "Mar",
                                                                    "Apr", "May", "Jun",
                                                                    "Jul", "Aug", "Sep",
                                                                    "Oct", "Nov", "Dec"))
louCrime$weekday <- factor(louCrime$weekday, levels = c("Sunday", "Monday", "Tuesday",
                                                        "Wednesday", "Thursday", "Friday",
                                                        "Saturday"))

