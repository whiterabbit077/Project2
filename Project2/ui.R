# ui.R
#load packages
library(shiny)
library(ggplot2)
library(ggcorrplot)
library(DT)
library(calendR)

# Define the UI for the Shiny app
shinyUI(fluidPage(
  titlePanel("Fruit Nutrition Analysis"), # Application title
  
  sidebarLayout(
    sidebarPanel(
      # Conditional panel for the Data Download tab
      conditionalPanel(
        condition = "input.tabs == 'Data Download'",
        selectInput("query_type", "Select Query Type:", choices = c("All Fruits", "Family by Name", "Order by Name", "Genus by Name", "Calories Range", "Fat Range", "Sugar Range", "Carbohydrates Range", "Protein Range")),
        # Show text input only for specific query types
        conditionalPanel(
          condition = "input.query_type != 'All Fruits' && input.query_type != 'Calories Range' && input.query_type != 'Fat Range' && input.query_type != 'Sugar Range' && input.query_type != 'Carbohydrates Range' && input.query_type != 'Protein Range'",
          textInput("query_param", "Enter Query Parameter:", value = "")
        ),
        # Show numeric inputs for range queries
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
        actionButton("query_button", "Query API"), # Button to query the API
        downloadButton("download_data", "Download Data") # Button to download data
      ),
      # Conditional panel for the About tab
      conditionalPanel(
        condition = "input.tabs == 'About'",
        plotOutput("calendarPlot")  # Display calendar plot
      ),
      # Conditional panel for the Data Exploration tab
      conditionalPanel(
        condition = "input.tabs == 'Data Exploration'",
        checkboxGroupInput("plots", "Select Plots to Display:", 
                           choices = c("Used Data" = "datatab",
                                       "Summary Stats Table" = "contingency",
                                       "Bar Plot" = "bar", 
                                       "Box Plot" = "box", 
                                       "Correlation Plot" = "corr",
                                       "Heatmap" = "heatmap"
                           )),
        uiOutput("familyInput"), # Dynamic UI for family selection
        actionButton("plot_button", "Generate Plots") # Button to generate plots
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs", # Tabset panel for different views
        
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
                 DTOutput("data_table") # Display data table
        ),
        
        tabPanel("Data Exploration",
                 conditionalPanel(
                   condition = "input.plot_button > 0",
                   uiOutput("selectedPlots") # Display selected plots
                 )
        )
      )
    )
  )
))
