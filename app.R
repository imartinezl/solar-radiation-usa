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
Sys.setenv(LANG = "en")

source('data_viz_functions.R', echo=F)

# Data Importation --------------------------------------------------------
print(getwd())
metadata <- read.csv('TMY3_StationsMeta.csv', stringsAsFactors = F) %>% 
  dplyr::mutate(id=1:n(),
                State_num = State %>% as.factor() %>% as.numeric(),
                data_file = paste0("alltmy3a/",USAF,"TYA.CSV"),
                label = paste0("<b>Station:</b> ", Site.Name, 
                               "<br/><b>State:</b> ", State, 
                               "<br/><b>Latitude:</b> ", Latitude, 
                               "<br/><b>Longitude:</b> ", Longitude,
                               "<br/><b>Class:</b> ", Class) )


# Define UI for application that draws a histogram
ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("sandstone"),#"journal"
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  title = "National Solar Radiation",
  
  shiny::titlePanel("National Solar Radiation - USA"),
  shiny::tags$p("Source:", shiny::a("TMY3 Dataset", href="https://rredc.nrel.gov/solar/old_data/nsrdb/1991-2005/tmy3/")),
  
  hr(),
  
  shiny::fluidRow(
    shiny::column(6,
                  leaflet::leafletOutput("map")
                  ),
    shiny::column(6,
                  shiny::tags$p("Click on any point on the map to access Solar Radiation information"),
                  shiny::uiOutput("select_ui"),
                  shiny::tableOutput('tbl'),
                  shiny::actionButton("pdf", "More about TMY3", onclick = "window.open('43156.pdf')")
    )
  ),
  hr(),
  shiny::fluidRow(
    shiny::column(4, 
                  shiny::tags$h4("Value along the year"),
                  shiny::plotOutput("plot_one_year", width = "100%", height = "600px")),
    shiny::column(4, 
                  shiny::tags$h4("Maximum value at each day"),
                  shiny::plotOutput("plot_maximum")),
    shiny::column(4, 
                  shiny::fluidRow(
                    shiny::column(8,shiny::tags$h4("Value on a specific day")),
                    shiny::column(4,shiny::uiOutput("date_ui"))
                  ),
                  shiny::plotOutput("plot_specific_day")
    )
  )
  
  
)
# pal <- leaflet::colorNumeric(palette = "magma", domain = metadata$State_num)
pal <- leaflet::colorNumeric(palette = c("#faa476","#f0746e","#e34f6f","#dc3977","#b9257a","#7c1d6f"), domain = metadata$State_num)
# pal <- leaflet::colorNumeric(palette = c("#7F3C8D","#11A579","#3969AC","#F2B701","#E73F74","#80BA5A","#E68310","#008695","#CF1C90","#f97b72","#4b4b8f","#A5AA99"), domain = metadata$State_num)
# pal <- leaflet::colorNumeric(palette = c("#f7feae","#b7e6a5","#7ccba2","#46aea0","#089099","#00718b","#045275"), domain = metadata$State_num)
# pal <- leaflet::colorNumeric(palette = c("#ff8484","#d84c73","#5c3b6f","#35234b"), domain = metadata$State_num)
# https://carto.com/carto-colors/


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 0, maxZoom = 18)) %>%
      leaflet::addProviderTiles(provider = leaflet::providers$Esri.WorldStreetMap, group = "Tiles") %>% 
      leaflet::addCircleMarkers(data = metadata, layerId = ~id, 
                                lat = ~ Latitude, lng = ~ Longitude, 
                                color= ~ pal(State_num), label = ~lapply(label, htmltools::HTML),
                                group = "Stations", radius = 2.5, stroke = T, weight = 2, 
                                opacity = 0.4, fillOpacity = 0.9) %>%
      leaflet::addLayersControl(
        overlayGroups = c("Tiles","Stations"),
        position = "topright",
        options = leaflet::layersControlOptions(collapsed = F)
      ) %>% 
      leaflet::addMiniMap(position = "bottomleft", toggleDisplay = T, minimized = F) %>%
      leaflet::setView(lat = 42.534924, lng = -100.089122, zoom = 3)
  })
  
  values <- shiny::reactiveValues(id = 263)
  shiny::observeEvent(input$map_marker_click,{
    click <- input$map_marker_click
    values$id <- click$id
    print(values$id)
  })
  
  
  shiny::observeEvent(values$id,{
    file <- metadata %>% dplyr::filter(id==values$id) %>% pull(data_file)
    metadata <- read.site.metadata(file)
    values$data <- read.data(file, metadata)
    output$tbl = shiny::renderTable(metadata %>% 
                                      dplyr::select(-site_id, -site_time_zone) %>% 
                                      `colnames<-`(c("Station","State","Latitude","Longitude","Elevation")))
    output$select_ui <- shiny::renderUI({
      choices <- colnames(values$data)[3:71]
      shiny::selectInput("variable", "Variable to plot", choices = choices, selected=1, width="150px")
    })
    
    initial_date <- values$data$date[1]
    all_dates <- seq(values$data$date %>% min(),values$data$date %>% max() , by="days")
    disable_dates <- all_dates[!(all_dates %in% values$data$date)]
    output$date_ui <- shiny::renderUI({
      shiny::dateInput("date", label = NULL, language = "en", weekstart = 1, 
                       value = initial_date, datesdisabled = disable_dates, width = "120px")
    })
  })
  shiny::observeEvent(input$variable,{
    if(!is.null(input$variable)){
      # variable_name <- bquote(ETR~Wh/m^2)
      variable_name <- NULL
      output$plot_one_year <- shiny::renderPlot({plot.oneYear(shiny::isolate(values$data),input$variable, variable_name)}, bg="transparent")
      output$plot_maximum <-  shiny::renderPlot({plot.maximum(shiny::isolate(values$data),input$variable, variable_name)}, bg="transparent")
    }
  })
  shiny::observeEvent({
    input$date
    },{
    if(!is.null(input$date)){
      variable_name <- NULL
      output$plot_specific_day <-  shiny::renderPlot({plot.specificDay(shiny::isolate(values$data), input$date, input$variable, variable_name)}, bg="transparent")
    }
  })
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)

