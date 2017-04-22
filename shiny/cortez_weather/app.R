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
library(dygraphs)

# Weather data
cortez_weather <- readr::read_csv("./data/cortez_weather.csv")
weather_stations <- readr::read_csv("./data/weather_stations.csv")
weather_stations_sp <- rgdal::readOGR(dsn = "./data/weather_stations.geojson", "OGRGeoJSON", verbose = FALSE)

the.height <- 250

# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("style.css"),
  dygraphOutput("temperature", height = the.height),
  dygraphOutput("precipitation", height = the.height)
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  cortez_weather_dy <- weather_stations %>%
    dplyr::filter(Location == "Cortez") %>%
    dplyr::select(DATE, TMAX_F, TMIN_F, FGDD, PRCP_IN) %>%
    as.data.frame()
  row.names(cortez_weather_dy) <- cortez_weather_dy$DATE
  
  output$temperature <- dygraphs::renderDygraph({
    dygraphs::dygraph(cortez_weather_dy %>% dplyr::select(TMAX_F, TMIN_F, FGDD),
                      group = "cortez",
                      height = the.height) %>%
      dygraphs::dyAxis('y',
                       label = "Temperature (ºF)",
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
      dygraphs::dyRangeSelector(dateWindow = c("2016-01-01", "2016-12-31"))
    
  })
  
  cortez_weather_dy$PRCP_IN2 <- cortez_weather_dy$PRCP_IN
  
  output$precipitation <- dygraphs::renderDygraph({
    dygraphs::dygraph(cortez_weather_dy %>% dplyr::select(PRCP_IN, PRCP_IN2),
                      group = "cortez",
                      height = the.height) %>%
      dygraphs::dyAxis('y',
                       label = "Precipitation (in)",
                       valueRange = c(0, NULL),
                       drawGrid = F,
                       independentTicks = T) %>%
      dygraphs::dyAxis('y2',
                       label = "Precipitation (in)",
                       valueRange = c(0, NULL),
                       drawGrid = F,
                       independentTicks = T) %>%
      dygraphs::dyAxis('x',
                       drawGrid = F) %>%
      dygraphs::dySeries("PRCP_IN", label = "Precipitation (in)", axis = 'y', stemPlot = T, drawPoints = F, strokeWidth = 1, pointSize = NA, color = "black") %>%
      dygraphs::dyLegend(show = "onmouseover") %>%
      dygraphs::dySeries("PRCP_IN2", label = "Precipitation (in) 2", axis = 'y2', stemPlot = T, drawPoints = F, strokeWidth = 0, pointSize = NA, color = "black") %>%
      dygraphs::dyRangeSelector(dateWindow = c("2016-01-01", "2016-12-31")) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

