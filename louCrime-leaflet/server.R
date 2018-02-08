shinyServer(function(input, output){
  
  # Geocoding location for base map
  #
  # With leaflet, I am not sure if this is necessary, but for filtering data to a 
  # quickly plottable level it is the best option I can think of right now
#   louisville_map <- eventReactive(input$update,{
#     data.frame(geocode(paste0(input$location, "Louisville, KY"), source = "google"))
#   })
  
   # Filtering Crime Data
  crime <- reactive({
    
   # Applying filters
    c <- louCrime %>%
      filter(
        year_occured ==input$year
      )
    
    # Location based filtering
    # Filtering results by those that fall in the bounding box of the current map 
    # turned out to be quite difficult
    # It is something I still want to do, but I am not sure how.
    
#     if(input$location %in% c$zip_code){
#       c <- c %>% filter(zip_code == input$location)
#     }
    
    # Optional: Filtering by Crime Type
    ifelse(is.null(input$crime) == TRUE, c,
           ifelse(input$crime == 'All', c, c <- c %>% filter(crime_type %in% input$crime)))
    
    # Optional: Filter by month
    ifelse(is.null(input$month) == TRUE, c,
           ifelse(input$month == 'All', c, c <- c %>% filter(month_occured == input$month)))
    
    # Optional: Filter by weekday
    ifelse(is.null(input$weekday) == TRUE, c,
           ifelse(input$weekday == 'All', c, c <- c %>% filter(weekday == input$weekday)))
    
    # Optional: Filter by Premise
    ifelse(is.null(input$premise) == TRUE, c,
           ifelse(input$premise == 'All', c, c <- c %>% filter(premise_type %in% input$premise)))
    c <- as.data.frame(c)
    c
    })
    
  

  output$map <- renderLeaflet({
#     louisville <- louisville_map()
#     
#     if(input$location == 'Louisville'){
#       leaflet()%>%
#         setView(lng = louisville[1], lat = louisville[2], zoom = 12)%>%
#         addTiles()%>%
#         addMarkers(data = crime(),
#                    popup = crime()$uor_desc,
#                    clusterOptions = markerClusterOptions()
#         )
#     }else{
#       leaflet()%>%
#         setView(lng = louisville[1], lat = louisville[2], zoom = 14)%>%
#         addTiles()%>%
#         addMarkers(data = crime(),
#                    popup = crime()$uor_desc,
#                    clusterOptions = markerClusterOptions()
#         )
#     }
    louisville <- geocode("Louisville", source = "dsk")
    
    leaflet()%>%
      setView(lng = louisville[1], lat = louisville[2], zoom = 13)%>%
      addTiles()%>%
      addMarkers(data = crime(),
                 popup = crime()$uor_desc,
                 clusterOptions = markerClusterOptions())
    
  })
  
  # Display of Selected Data -- showing only data currently displayed in the Map Tab
  output$plotdata <- renderDataTable({
    
    display_crime <- crime()
    
    display_crime %>%
      select(date_occured, premise_type, full_address, uor_desc, offense = nibrs_offenses)
  }, options = list(pageLength = 10, lengthMenu = c(10, 20), lengthChange =TRUE, autoWidth = TRUE))
  
  output$n_crimes <- renderText({
    displayed.crimes <- nrow(crime())
    paste0(displayed.crimes, " crimes displayed for the selected metrics.")
  })
  
  
 })