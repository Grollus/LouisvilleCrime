# library(data.table)
library(dplyr)
library(ggplot2)
library(ggmap)
library(grid)
library(RColorBrewer)
# Read data in from csv
# louCrime <- fread('lou_shiny_data.csv', stringsAsFactors = FALSE, data.table = FALSE)
louCrime <- readRDS("Data/lou_shiny_data.rds")
louCrime <- as.data.frame(louCrime)
louCrime$month_occured <- factor(louCrime$month_occured, levels = c("Jan", "Feb", "Mar",
                                                                    "Apr", "May", "Jun",
                                                                    "Jul", "Aug", "Sep",
                                                                    "Oct", "Nov", "Dec"))
louCrime$weekday <- factor(louCrime$weekday, levels = c("Sunday", "Monday", "Tuesday",
                                                        "Wednesday", "Thursday", "Friday",
                                                        "Saturday"))

shinyServer(function(input, output){
  
  # Geocode input location for base maps
  bMap_geocode <- reactive({
    data.frame(geocode = geocode(paste(input$location, "Louisville, KY"), source = "google"))
    
  })
  
  # Create base map;
    # @ location is reactive
    # @ zoom is reactive
  baseMap <- reactive({
    
    temp_bMap_geocode <- bMap_geocode()
    
    base <- get_map(location = as.matrix(temp_bMap_geocode), source = "google",
                    zoom = input$zoom, # 10 is default city view. 11 seems better 
                    maptype = "roadmap",
                    color = "bw",
                    scale = 2) # high res image significantly clearer
    base
  })
  
  
  # Filtering Crime Data, return data frame
  crime <- reactive({
    
    # Grab the boundaries of the base map from baseMap
    # Use these bounds to filter data based on inclusion in map frame
    temp_map <- isolate(baseMap())
    bounds <- attr(temp_map, "bb")
    
    
    # Apply filters
    c <- louCrime %>%
      filter(
        year_occured >= input$year[1],
        year_occured <= input$year[2]
      )
    # Currently, default will show all the data with 'Louisville' coords as map center
    # If any other location is entered, the map will center at entered location
    #   and any data that wouldn't fit in frame is filtered out.
    #   Zooming will also filter out data as it is displaced by the map edges.
    
    # Zip code filtering below was removed for the time being.  This filtered to data only
    #   in the desired zip code, while centering the map at the zipcode.
    #else if (input$location %in% c$zip_code){
    # c <- c %>% filter(zip_code == input$location)
    if(input$location == "Louisville" & input$zoom == 11){
      c
    }else if(input$location %in% c$zip_code){
      c <- c %>% filter(zip_code == input$location)
    }else{
      c <- c %>% filter(lat >= bounds$ll.lat &
                        lat <= bounds$ur.lat &
                        lng >= bounds$ll.lon &
                        lng <= bounds$ur.lon)
    }
    
    # Optional: filter by crime type
    ifelse(input$crime != "All", c <- c %>% filter(crime_type %in% input$crime), c)
    
    # Optional: filter by month
    if(input$month != "All"){
      c <- c %>% filter(month_occured == input$month)
    }
    
    # Optional: filter by weekday
    if(input$weekday != "All"){
      c <- c %>% filter(weekday == input$weekday)
    }
    
    # Optional: filter by premise type
    ifelse(input$premise != "All", c <- c %>% filter(premise_type %in% input$premise), c)
    
    c <- as.data.frame(c)
    c
  })
  
  
  # Plotting Louisville crime map
  output$map <- renderPlot({
    
    
    
    # add Cartesian coordinates to enable more geoms
    
    bMap <- ggmap(baseMap(), extent = "panel") + coord_cartesian() 
    
    # Main ggplot object
    if(input$zoom == 15){
      colorCount <- length(unique(crime()$crime_type))
      
      mapFinal <- bMap +
        geom_point(data = crime(), aes(x = lng, y = lat,
                                       color = crime_type,
                                       size = full_address, 
                                       alpha = .4
                   )) +
        guides(alpha = FALSE, color = guide_legend(override.aes = list(size = 4))) + 
        scale_color_manual(labels = c("Assault", "Burglary", "DTP", "D/A", 
                                      "DUI", "Fraud", "Homicide", "MVT", "Other", 
                                      "Robbery", "Sex Crimes", "Theft", "Vandalism",
                                      "Vehicle Break In", "Weapon"), 
                           values = colorRampPalette(brewer.pal(12, "Blues"))(colorCount)) + 
        scale_size_discrete(guide = FALSE, range = c(0, 20)) +
        labs(x = "Longitude", y = "Latitude") +
        ggtitle(paste(nrow(crime()), " crimes displayed over the period from ", input$year[1], " to ",
                      input$year[2], sep = "")) +
        theme_bw() +
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          legend.position = "bottom"
        )
    }else {
    
    mapFinal <- bMap +
      
      # Default setting is a density estimation polot
      # This seems to break when zoomed in too far
      # So will adjust to point plotting when zoomed in to the max.
      stat_density2d(data = crime(), aes(x = lng,
                     y = lat,
                     fill = ..level.., # Not sure about this argument in geom_point
                     alpha = ..level..),
                     geom = "polygon",
                     bins = input$bins,
                     size = input$boundwidth,
                     colour = "ivory") +
      
      # Configuring scale and panel
      scale_alpha(range = input$alpharange) +
      scale_fill_gradient(low = input$low, high = input$high) +
      
      # Title and labels
      labs(x = "Longitude", y = "Latitude") + 
      ggtitle(paste(nrow(crime()), " crimes displayed over the period from ", input$year[1], " to ", 
                    input$year[2], sep = "")) + 
      
      # Other plot details
      theme_bw() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = "none"
      )
    }
    # Faceting 
    if(input$facet == "no faceting") {mapFinal}
    if(input$facet == "month") {mapFinal <- mapFinal + facet_wrap(~ month_occured)}
    if(input$facet == "year") {mapFinal <- mapFinal + facet_wrap(~ year_occured)}
    if(input$facet == "weekday") {mapFinal <- mapFinal + facet_wrap(~ weekday)}
    suppressWarnings(print(mapFinal))
  })
  
  # Display of Selected Data--shows only data currently displayed in the Map Tab
  output$plotdata <- renderDataTable({
    
    display_crime <- crime()
    
    display_crime %>%
      select(date_occured, premise_type, full_address, uor_desc, offense = nibrs_offenses)
    
  }, options = list(lengthMenu = list(c(15, 30), c('15', '30')), pageLength = 15))
  
  
})