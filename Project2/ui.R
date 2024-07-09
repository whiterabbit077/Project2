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
        selectInput("query_type", "Select Query Type:", choices = c("All Fruits", "Family by Name", "Order by Name", "Genus by Name", "Calories Range", "Fat Range", "Sugar Range", "Carbohydrates Range", "Protein Range")),
        conditionalPanel(
          condition = "input.query_type != 'All Fruits' && input.query_type != 'Calories Range' && input.query_type != 'Fat Range' && input.query_type != 'Sugar Range' && input.query_type != 'Carbohydrates Range' && input.query_type != 'Protein Range'",
          textInput("query_param", "Enter Query Parameter:", value = "")
        ),
        conditionalPanel(
          condition = "input.query_type == 'Calories Range'",
          numericInput("min_calories", "Min Calories:", value = 0),
          numericInput("max_calories", "Max Calories:", value = 1000)
        ),
        conditionalPanel(
          condition = "input.query_type == 'Fat Range'",
          numericInput("min_fat", "Min Fat:", value = 0),
          numericInput("max_fat", "Max Fat:", value = 1000)
        ),
        conditionalPanel(
          condition = "input.query_type == 'Sugar Range'",
          numericInput("min_sugar", "Min Sugar:", value = 0),
          numericInput("max_sugar", "Max Sugar:", value = 1000)
        ),
        conditionalPanel(
          condition = "input.query_type == 'Carbohydrates Range'",
          numericInput("min_carbohydrates", "Min Carbohydrates:", value = 0),
          numericInput("max_carbohydrates", "Max Carbohydrates:", value = 1000)
        ),
        conditionalPanel(
          condition = "input.query_type == 'Protein Range'",
          numericInput("min_protein", "Min Protein:", value = 0),
          numericInput("max_protein", "Max Protein:", value = 1000)
        ),
        actionButton("query_button", "Query API"),
        downloadButton("download_data", "Download Data")
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
                 img(src = "fruit_image.jpg", height = "300px")  # Ensure this path is correct
        ),
        
        tabPanel("Data Download", 
                 DTOutput("data_table")
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
