# ui.R
library(shiny)
library(ggplot2)
library(ggcorrplot)

shinyUI(fluidPage(
  titlePanel("Fruit Nutrition Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("family", "Select Family:", choices = c("All", unique(data$family))),
      actionButton("plot_button", "Generate Plots")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Description", 
                 h4("Dataset Description"),
                 p("This dataset contains nutritional information of various fruits...")),
        tabPanel("Plots",
                 conditionalPanel(
                   condition = "input.plot_button > 0",
                   plotOutput("barPlot"),
                   plotOutput("boxPlot"),
                   plotOutput("corrPlot"),
                   textOutput("warningText")
                 )
        )
      )
    )
  )
))
