library(ggplot2)

shinyUI(fluidPage(
  titlePanel("Louisville Crime Mapping"),
  fluidRow(
    column(3,
           wellPanel(
             h4("Filter"),
             textInput("location", "Enter a place of interest:", "Louisville"),
             helpText("Examples: 40211, Shively, 	101 E Main St, etc."),
             sliderInput("year", "Display Years",
                         min = 1920, max = 2015, value = c(2014, 2015), step = 1),
             selectInput("month", "Display Months", 
                         choices = c("All", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), # TODO expand to seasons. Need basic functionality first
             selectInput("weekday", "Day(s) of Week",
                         choices = c("All", "Sunday", "Monday", "Tuesday", "Wednesday",
                                     "Thursday", "Friday", "Saturday")),
             selectInput("crime", "Select crime to display", multiple = TRUE,
                                choices = c("All", "arson", "assault", "burglary",
                                            "disturbing the peace", "drugs/alcohol violations",
                                            "dui", "fraud", "homicide", "motor vehicle theft",
                                            "other", "robbery", "sex crimes", "theft/larceny",
                                            "vandalism", "vehicle break-in/theft", "weapons"),
                                selected = "All"),
             selectInput("premise", "Crime Premise Type", multiple = TRUE, selected = "All",
                         choices  = c("All", "abandoned/condemned structure", "air / bus / train terminal",
                                      "amusement park", "atm separate from bank", "attached residential garage",
                                      "auto dealership (new or used)", "bank / savings & loan",
                                      "bar / night club", "camp / campground",
                                      "child daycare facility", "church / synagogue / temple",
                                      "commercial / office building", "construction site",
                                      "convenience store", "department / discount store",
                                      "dock/wharf/freight/modal terminal", "drug store/dr`s office/hospital",
                                      "fairgrounds / stadium / arena", "farm facility",
                                      "field / woods", "government / public building", 
                                      "grocery / supermarket", "highway / road / alley",
                                      "homeless shelter / mission", "hotel / motel / etc.",
                                      "industrial site", "jail / penitentary", "lake / waterway",
                                      "liquor store", "mall / shopping center", "military installation",
                                      "non attached resd garage/shed/buld", "other / unknown",
                                      "other residence (apartment/condo)", "park / playground",
                                      "parking lot / garage", "race track/gambling facility",
                                      "rental / storage facility", "residence / home",
                                      "rest area", "restaurant", "school - college / university",
                                      "school - elementary / secondary", "service / gas station",
                                      "specialty store (tv, fur, etc)", "tribal lands")),
             selectInput("facet", "Facet crime by:", choice = c("no faceting", "year", "month", "weekday")),
             tags$div(title = "Click here to replot after adjusting any input",
                      submitButton("Filter and Plot")
                      )
             
            )),
  mainPanel(
    tabsetPanel(
      tabPanel("Map", plotOutput("map", width = "775px", "775px")),
      tabPanel("Displayed Data", dataTableOutput("plotdata"))
    )
      
    )),
  fluidRow(
    column(4,
           sliderInput("zoom", label = "Zoom",
                       min = 9, max = 15, value = 11),     
           conditionalPanel(
             condition = "input.zoom < 15",
             # Settings for density plot
             sliderInput("alpharange", "Alpha Range:", 
                         min = 0, max = 1, step = 0.1, value = c(0.1, 0.4)))
           
    ),
    column(4,
           conditionalPanel(
             condition = "input.zoom < 15",
             sliderInput("bins", "Number of Bins:", 
                         min = 5, max = 50, step = 5, value = 15),
             sliderInput("boundwidth", "BoundaryLines Width:",
                         min = 0, max = 1, step = 0.1, value = 0.1))
           
    ),
    column(4,
           conditionalPanel(
             condition = "input.zoom < 15",
             selectInput("low", "Fill Gradient (Low):",
                         choices = c("yellow", "red", "orange", "black", "white", "blue",
                                     "green", "purple")),
             br(),
             br(),
             selectInput("high", "Fill Gradient (High):",
                         choices = c("red", "orange", "black", "white", "blue", "green",
                                     "purple", "yellow"))
           )
           )
           
  )
))