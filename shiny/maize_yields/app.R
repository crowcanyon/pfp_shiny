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

growth_summaries <- readr::read_csv("./data/growth_summaries.csv")
ears <- readr::read_csv("./data/ears.csv")
yields <- readr::read_csv("./data/yields.csv")

cortez_weather <- readr::read_csv("./data/cortez_weather.csv")

pdf(NULL)

# Define UI for application that draws a histogram
ui <- 
  fluidPage(
    includeCSS("style.css"),
    
    fluidRow(
      column(4,
             shiny::selectInput("the_var_yield",
                                label = "Measurement:",
                                choices = c("Kernel yield",
                                            "Cob yield",
                                            "Ear yield"),
                                selected = "Kernel yield")
      ),
      column(4,
             shiny::selectInput("color_yield",
                                label = "Color by:",
                                choices = c("Garden" = "Garden",
                                            "Maize variety" = "Variety"))
      )
    ),
    
    plotOutput("yieldPlot")
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$yieldPlot <- shiny::renderPlot({
    
    yield_plot <- ggplot2::ggplot() + 
      ggplot2::ggtitle(input$the_var_yield) +
      ggplot2::ylab(paste0(input$the_var_yield," (kg/ha)")) +
      ggplot2::theme(axis.text = ggplot2::element_text(size=14),
                     axis.title = ggplot2::element_text(size=16,face="bold"),
                     legend.title = ggplot2::element_text(size=16,face="bold"),
                     legend.text = ggplot2::element_text(size=14),
                     plot.title = ggplot2::element_text(size=18,face="bold"))
    
    yield_plot <- yield_plot + ggplot2::geom_point(data = yields, na.rm = T, ggplot2::aes(y = get(input$the_var_yield), x = Season, colour = get(input$color_yield)), size=4) +
      ggplot2::ylab(paste0(input$the_var_yield," (kg/ha)")) +
      ggplot2::scale_colour_discrete(name = input$color_yield)
    
    yield_plot
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

