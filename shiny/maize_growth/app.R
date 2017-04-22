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
             selectInput("the_year",
                         label = "Growing season:",
                         choices = 2016:2009,
                         selected = 2016,
                         width = 200)
      ),
      column(4,
             selectInput("the_var",
                         label = "Developmental stage:",
                         choices = c("Early tassel development" = "Early Tassel Development",
                                     "Tassel development" = "Tassel Development",
                                     "Tasseling" = "Tasseling",
                                     "Silk development" = "Silk Development",
                                     "Silking" = "Silking",
                                     "Ear development" = "Ear Development"),
                         width = 200)
      ),
      column(4,
             selectInput("x_axis",
                         label = "Horizontal axis units:",
                         choices = c("Date",
                                     "Accumulated FGDD"),
                         width = 200)
      )
    ),
    
    # selectInput("color",
    #                    label = "Color by:",
    #                    choices = c("Garden" = "Garden",
    #                                "Maize variety" = "Variety"),
    #                    width = 200),
    # 
    
    plotOutput("growthPlot")
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$growthPlot <- shiny::renderPlot({

    growth_plot <- growth_summaries %>%
      dplyr::filter(lubridate::year(Date) == input$the_year)
    
    # if(input$show_precip){
    weather_plot <- cortez_weather %>%
      dplyr::filter(DATE %within% (min(growth_plot$Date)%--%max(growth_plot$Date)))
    # }
    
    p <- ggplot2::ggplot() +
      ggplot2::ylim(0,1) +
      ggplot2::ggtitle(input$the_var) +
      ggplot2::theme(axis.text = ggplot2::element_text(size=14),
                     axis.title = ggplot2::element_text(size=16,face="bold"),
                     legend.title = ggplot2::element_text(size=16,face="bold"),
                     legend.text = ggplot2::element_text(size=14),
                     plot.title = ggplot2::element_text(size=18,face="bold"))
    
    if(input$x_axis == "Date"){
      p <- p + ggplot2::xlab("Date")
      
      # if(input$show_precip){
      p <- p + 
        ggplot2::geom_linerange(data = weather_plot, ggplot2::aes(x=DATE, ymin = 0, ymax = PRCP_IN)) + 
        ggplot2::ylab("Proportion of clumps with plants at stage\n —and— \n Daily precipitation in Cortez (inches)")
      # }else{
      #   p <- p + ggplot2::ylab("Prop. of clumps with plants at stage")
      # }
      
      p <- p + 
        ggplot2::geom_line(data=growth_plot, ggplot2::aes(lubridate::as_date(Date),
                                                          get(input$the_var),
                                                          group = Garden,
                                                          # colour = get(input$color)
                                                          colour = get("Garden")
                                                          )) +
        ggplot2::scale_x_date(breaks = seq(
          lubridate::as_date(min(growth_plot$Date)),
          lubridate::as_date(lubridate::mdy(paste0("1015",input$the_year))),
          by = '2 weeks'), 
          date_labels = "%b %d", 
          limits = c(lubridate::as_date(min(growth_plot$Date)),
                     lubridate::as_date(lubridate::mdy(paste0("1015",input$the_year)))))
      
    }else if(input$x_axis == "Accumulated FGDD"){
      
      p <- p + 
        ggplot2::xlab("Fahrenheit Growing Degree Days since planting") +
        ggplot2::ylab("Proportion of clumps with plants at stage") + 
        ggplot2::geom_line(data=growth_plot,
                           ggplot2::aes(Acc_FGDD,
                                        get(input$the_var),
                                        group=Garden,
                                        # colour=get(input$color)
                                        colour=get("Garden")
                                        )) +
        ggplot2::scale_x_continuous(breaks = seq(0,3000,500), limits = c(0,3000))
    }
    
    p <- p +
      ggplot2::scale_colour_discrete(name = input$color)
    
    p
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

