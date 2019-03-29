#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(zeallot)
library(shiny)
Sys.setlocale("LC_TIME", "en_US.UTF-8") 
Sys.getlocale("LC_TIME")

# Data Importation --------------------------------------------------------
print(getwd())
metadata <- read.csv('TMY3_StationsMeta.csv', stringsAsFactors = F) %>% 
  dplyr::mutate(id=1:n(),
                State_num = State %>% as.factor() %>% as.numeric(),
                data_file = paste0("allmy3a/",USAF,"TYA.CSV"))


# Define UI for application that draws a histogram
ui <- shiny::fluidPage(
   
   # Application title
  shiny::titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   shiny::sidebarLayout(
     shiny::sidebarPanel(
       shiny::verbatimTextOutput("Click_text")
      ),
      
      # Show a plot of the generated distribution
      shiny::mainPanel(
        leaflet::leafletOutput("map")
      )
   ),
  DT::DTOutput('tbl')
)
pal <- leaflet::colorNumeric(palette = "plasma", domain = metadata$State_num)
pal <- leaflet::colorNumeric(palette = c("#fcde9c","#faa476","#f0746e","#e34f6f","#dc3977","#b9257a","#7c1d6f"), domain = metadata$State_num)
# https://carto.com/carto-colors/

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  values <- shiny::reactiveValues()
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 0, maxZoom = 18)) %>%
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "Tiles") %>% 
      leaflet::addCircleMarkers(data = metadata, layerId = ~id, lat = ~ Latitude, lng = ~ Longitude, group = "Points",
                                radius = 2.5, stroke = T, weight = 2, color= ~ pal(State_num), opacity = 0.4) %>%
      leaflet::addLayersControl(
        overlayGroups = c("Tiles","Points"),
        position = "topright",
        options = leaflet::layersControlOptions(collapsed = F)
      ) %>% 
      leaflet::addMiniMap(position = "bottomleft", toggleDisplay = T, minimized = T) %>%
      leaflet::setView(lat = 42.534924, lng = -100.089122, zoom = 3)
  })
  
  observe({
    click<-input$map_marker_click
    if(is.null(click))
      return()
    text<-paste("ID: ", click$id, "Latitude ", click$lat, "Longitude ", click$lng)
    output$Click_text <- shiny::renderText({
      text
    })
    values$data <- read.csv(metadata %>% dplyr::filter(id==click$id) %>% pull(data_file), skip = 1)
    if(nrow(values$data)==0)
      return()
    output$tbl = DT::renderDT(
      values$data, options = list(lengthChange = FALSE)
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

