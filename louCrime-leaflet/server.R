shinyServer(function(input, output){
  
  # Geocoding location for base map
  #
  # With leaflet, I am not sure if this is necessary, but for filtering data to a 
  # quickly plottable level it is the best option I can think of right now
  louisville_map <- reactive({
    data.frame(geocode(paste0(input$location, "Louisville, KY"), source = "google"))
  })
  
  # Creating a reactive google map so I can grab bounding box coordinates. 
  # Not elegant, but I am not sure how to filter the data by location otherwise.
  # map itself is not used.
  
  map_for_bb_coords <- reactive({
    temp_map <- louisville_map()
    bb_map <- get_map(location = as.matrix(temp_map), source = "google")
    bb_map
  })
  
  # Filtering Crime Data
  crime <- reactive({
    
    #extracting boundaries from google map element
    temp_map <- isolate(map_for_bb_coords())
    bounds <- attr(temp_map, 'bb')
  
    # Applying filters
    c <- louCrime %>%
      filter(
        year_occured >= input$year[1],
        year_occured <= input$year[2]
      )
    
    # Location based filtering
    if(input$location == "Louisville"){
      c
    }else if(input$location %in% c$zip_code){
      c <- c %>% filter(zip_code == input$location)
    }else{
      c <- c %>% filter(lat >= bounds$ll.lat &
                        lat <= bounds$ur.lat &
                        lng >= bounds$ll.lon &
                        lng <= bounds$ur.lon)
    }
    
    c <- as.data.frame(c)
    
    # Leaflet markers only really work well(without tinkering) when points < 100,000
    # check the number of points in the data frame and if above threshold, randomly
    # sample the data to plot
    if(nrow(c) > 80000){
      c <- sample_n(c, 80000, replace = FALSE)
    }
    
    c
    })
  
  
  
  output$map <- renderLeaflet({
    louisville <- louisville_map()
    leaflet()%>%
      setView(lng = louisville[1], lat = louisville[2], zoom = 12)%>%
      addTiles()%>%
      addMarkers(data = crime(),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Display of Selected Data -- showing only data currently displayed in the Map Tab
  output$plotdata <- renderDataTable({
    
    display_crime <- crime()
    
    display_crime %>%
      select(date_occured, premise_type, full_address, uor_desc, offense = nibrs_offenses)
  }, options = list(lengthMenu = list(c(15, 30), c('15', '30')), pageLength = 15))
  
})