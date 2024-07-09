# ui.R
library(shiny)
library(ggplot2)
library(ggcorrplot)
library(DT) 

shinyUI(fluidPage(
  titlePanel("Fruit Nutrition Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Data Download'",
        textInput("api_url", "API URL:", value = "https://www.fruityvice.com/api/fruit/all"),
        actionButton("download_button", "Download Data")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Data Exploration'",
        selectInput("family", "Select Family:", choices = c("All", unique(data$family))),
        actionButton("plot_button", "Generate Plots")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        
        tabPanel("About", 
                 h4("About the App"),
                 p("This app provides an analysis of nutritional data for various fruits."),
                 p("Data Source: ", a("Fruityvice API", href = "https://www.fruityvice.com/")),
                 p("Tabs Description:"),
                 tags$ul(
                   tags$li("About: Describes the purpose of the app and the data source."),
                   tags$li("Data Download: Allows the user to download and subset data."),
                   tags$li("Data Exploration: Allows the user to visualize and analyze the data.")
                 ),
                 img(src = "fruit_image.jpg", height = "400px")
        ),
        
        tabPanel("Data Download", 
                 DTOutput("data_table"),
                 downloadButton("download_data", "Download Data")
        ),
        
        tabPanel("Data Exploration",
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
