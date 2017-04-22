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

soil_moisture <- readr::read_csv("./data/soil_moisture.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("style.css"),
  fluidRow(
    column(4,
           shiny::selectInput("the_var_soil_moisture", label = "Soil measurement:",
                              choices = c("Moisture","Temperature"))
           ),
    column(4,
           shiny::selectInput("the_garden_soil_moisture", label = "Garden/Location:",
                              choices = unique(gsub(" — NA","",paste0(soil_moisture$Garden," — ",soil_moisture$Location))))
           ),
    column(4,
           shiny::checkboxInput("freeze_y_soil_moisture", label = "Freeze y scale?", value = TRUE)
           )
  ),
  # hr(),
  # Show a plot of the generated distribution
  
  plotOutput("soilPlot")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$soilPlot <- renderPlot({
    garden <- strsplit(input$the_garden_soil_moisture," — ")[[1]][1]
    loc <- strsplit(input$the_garden_soil_moisture," — ")[[1]][2]
    
    moisture_data <- soil_moisture %>%
      dplyr::filter(Garden == garden)
    
    if(!is.na(loc)){
      moisture_data <- moisture_data %>%
        dplyr::filter(Location == loc)
    }
    
    if(input$the_var_soil_moisture == "Moisture"){
      moisture_data <- moisture_data %>%
        dplyr::select(Time,contains("VWC")) %>%
        reshape2::melt(id.vars="Time")
      
      the_range <- c(0.05,0.35)
    }else{
      moisture_data <- moisture_data %>%
        dplyr::select(Time,contains("Temp")) %>%
        reshape2::melt(id.vars="Time")
      
      the_range <- c(32,85)
    }
    
    # xrange <- range(moisture_data$Time)
    xrange <- as.POSIXct(c("2015-06-01 UTC","2016-12-31 UTC"))
    
    soil_moisture_plot <- ggplot2::ggplot() + 
      ggplot2::ggtitle(paste0("Soil ",input$the_var_soil_moisture,": ", input$the_garden_soil_moisture)) +
      ggplot2::theme(axis.text = ggplot2::element_text(size=14),
                     axis.title = ggplot2::element_text(size=16,face="bold"),
                     legend.title = ggplot2::element_text(size=16,face="bold"),
                     legend.text = ggplot2::element_text(size=14),
                     plot.title = ggplot2::element_text(size=18,face="bold")) + 
      ggplot2::xlab("Date") + 
      ggplot2::xlim(xrange)
    
    if(input$freeze_y_soil_moisture){
      soil_moisture_plot <- soil_moisture_plot +
        ggplot2::ylim(the_range)
    }
    
    soil_moisture_plot <- soil_moisture_plot + 
      ggplot2::geom_line(data = moisture_data,
                         ggplot2::aes(y = value, 
                                      x = Time,
                                      col=variable
                         ),
                         size=2)
    
    if(input$the_var_soil_moisture == "Moisture"){
      soil_moisture_plot <- soil_moisture_plot + 
        ggplot2::ylab(bquote('Volumetric water content ('*m^3/m^3*')')) +
        ggplot2::scale_colour_hue(name="Depth",
                                  breaks=c("VWC_15", "VWC_30", "VWC_45"),
                                  labels=c('15 cm', '30 cm', '45 cm'))
      
      soil_moisture_plot <- soil_moisture_plot + 
        ggplot2::ylab(bquote('Volumetric water content ('*m^3/m^3*')'))
      
    }else{
      soil_moisture_plot <- soil_moisture_plot + 
        ggplot2::ylab("Temperature (ºF)") +
        ggplot2::scale_colour_hue(name="Depth",
                                  breaks=c("Temp_15", "Temp_30", "Temp_45"),
                                  labels=c('15 cm', '30 cm', '45 cm'))
    }
    
    soil_moisture_plot
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

