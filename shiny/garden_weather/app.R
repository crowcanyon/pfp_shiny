#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
library(dplyr)
library(lubridate)
library(sp)
library(htmlwidgets)
library(leaflet)
library(dygraphs)

# Weather data
cortez_weather <- readr::read_csv("./data/cortez_weather.csv")
weather_stations <- readr::read_csv("./data/weather_stations.csv")
weather_stations_sp <- rgdal::readOGR(dsn = "./data/weather_stations.geojson", "OGRGeoJSON", verbose = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("style.css"),
    fluidRow(
      column(12,
             selectInput("the_garden_weather", label = "Garden/Location:",
                         choices = unique((weather_stations %>%
                                             dplyr::select(Location) %>%
                                             dplyr::filter(!(Location %in% c("Cortez","East_Hill"))))$Location))
             )

    ),
    # hr(),
    # Show a plot of the generated distribution

      dygraphOutput("distPlot")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    weather_stations_dy <- reactive({
      weather_stations %>%
      dplyr::select(Location,DATE, TMAX_F, TMIN_F, FGDD) %>%
      dplyr::filter(Location == input$the_garden_weather) %>%
      as.data.frame()
      
    })
    
    dates <- reactive({
      weather_stations %>%
        dplyr::select(Location,DATE, TMAX_F, TMIN_F, FGDD) %>%
        dplyr::filter(Location == input$the_garden_weather) %>%
        as.data.frame() %$%
        DATE
    })

    output$distPlot <- renderDygraph({
      dygraphs::dygraph(weather_stations_dy() %>% 
                          magrittr::set_rownames(dates()) %>%
                          dplyr::select(TMAX_F, TMIN_F, FGDD),
                      group = input$the_garden_weather) %>%
      dygraphs::dyAxis('y',
                       label = "Temperature (ºF)",
                       valueRange = c(-30, 125),
                       drawGrid = F,
                       independentTicks = T) %>%
      dygraphs::dyAxis('y2',
                       label = "Daily GDD (F)",
                       valueRange = c(0, 30),
                       drawGrid = F,
                       independentTicks = T) %>%
      dygraphs::dyAxis('x',
                       drawGrid = F) %>%
      dygraphs::dySeries("TMAX_F", label = "Maximum temperature (ºF)", axis = 'y', color = "#ef8a62", strokeWidth = 1.5) %>%
      dygraphs::dySeries("TMIN_F", label = "Minimum temperature (ºF)", axis = 'y', color = "#67a9cf", strokeWidth = 1.5) %>% 
      dygraphs::dySeries("FGDD", label = "GDD (F)", axis = "y2", color = "black") %>%
      dygraphs::dyLegend(labelsSeparateLines = T, show = "onmouseover") %>%
      dygraphs::dyRangeSelector(dateWindow = c("2016-09-01", "2016-10-31"))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

