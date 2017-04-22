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

# Define UI for application that draws a histogram
ui <- 
  fluidPage(
    includeCSS("style.css"),
    
    fluidRow(
      column(4,
             shiny::selectInput("the_year_ears",
                                label = "Growing season:",
                                choices = 2016:2009,
                                selected = 2016)
      ),
      column(4,
             shiny::selectInput("the_var_ears",
                                label = "Measurement:",
                                choices = c("Kernel weight",
                                            "Cob weight",
                                            "Ear weight"),
                                selected = "Kernel weight"
             )
      ),
      column(4,
             shiny::selectInput("color_ears",
                                label = "Color by:",
                                choices = c("Garden" = "Garden",
                                            "Maize variety" = "Variety"))
      )
    ),
    
    # selectInput("color",
    #                    label = "Color by:",
    #                    choices = c("Garden" = "Garden",
    #                                "Maize variety" = "Variety"),
    #                    width = 200),
    # 
    
    plotOutput("earPlot")
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$earPlot <- shiny::renderPlot({
    
    ymax <- ears %>% 
      select(get(input$the_var_ears)) %>% 
      max(na.rm = T) %>% 
      reshape::round_any(accuracy = 50, f = ceiling)
    
    ear_plot <- ggplot2::ggplot() + 
      ggplot2::ggtitle(input$the_var_ears) +
      ggplot2::expand_limits(y=0) +
      ggplot2::theme(axis.text = ggplot2::element_text(size=14),
                     axis.title = ggplot2::element_text(size=16,face="bold"),
                     legend.title = ggplot2::element_text(size=16,face="bold"),
                     legend.text = ggplot2::element_text(size=14),
                     plot.title = ggplot2::element_text(size=18,face="bold"))
    
    
    # if(input$freeze_y_ears){
    ear_plot <- ear_plot + 
      ggplot2::ylim(c(0,ymax))
    # }
    
    ear_plot <- ear_plot +  ggplot2::geom_boxplot(data = ears %>%
                                                    dplyr::filter(Season == input$the_year_ears),
                                                  ggplot2::aes(Garden, 
                                                               get(input$the_var_ears), 
                                                               fill = get(input$color_ears)
                                                  )
    ) +
      ggplot2::ylab(paste0(input$the_var_ears," (g)"))
    
    ear_plot <- ear_plot +
      ggplot2::labs(fill = input$color_ears)
    
    ear_plot
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

