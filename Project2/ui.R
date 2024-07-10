# ui.R
# Load necessary libraries
library(shiny)
library(ggplot2)
library(ggcorrplot)
library(DT)
library(calendR)

# Define the UI for the Shiny app
shinyUI(fluidPage(
  titlePanel("Fruit Nutrition Analysis"), # Application title
# Define the UI of the Shiny application
shinyUI(fluidPage(
  # Add a title panel to the UI
  titlePanel("Fruit Nutrition Analysis"),

  # Set up a layout with a sidebar and main panel
  sidebarLayout(
    sidebarPanel(
      # Conditional panel for the Data Download tab
      conditionalPanel(
        condition = "input.tabs == 'Data Download'",
        selectInput("query_type", "Select Query Type:", choices = c("All Fruits", "Family by Name", "Order by Name", "Genus by Name", "Calories Range", "Fat Range", "Sugar Range", "Carbohydrates Range", "Protein Range")),
        # Show text input only for specific query types

      # Add conditional panels that display based on the selected tab
      conditionalPanel(
        condition = "input.tabs == 'Data Download'",
        
        # Dropdown menu for selecting the type of query
        selectInput("query_type", "Select Query Type:", 
                    choices = c("All Fruits", "Family by Name", "Order by Name", 
                                "Genus by Name", "Calories Range", "Fat Range", 
                                "Sugar Range", "Carbohydrates Range", "Protein Range")),
        
        # Text input for query parameters when specific query types are selected
        conditionalPanel(
          condition = "input.query_type != 'All Fruits' && input.query_type != 'Calories Range' && 
                      input.query_type != 'Fat Range' && input.query_type != 'Sugar Range' && 
                      input.query_type != 'Carbohydrates Range' && input.query_type != 'Protein Range'",
          textInput("query_param", "Enter Query Parameter:", value = "")
        ),

        conditionalPanel(
          condition = "input.query_type == 'Calories Range'",
          numericInput("min_calories", "Min Calories:", value = 0),
          numericInput("max_calories", "Max Calories:", value = 1000)
        ),
        
        # Numeric inputs for fat range query
        conditionalPanel(
          condition = "input.query_type == 'Fat Range'",
          numericInput("min_fat", "Min Fat:", value = 0),
          numericInput("max_fat", "Max Fat:", value = 1000)
        ),
        
        # Numeric inputs for sugar range query
        conditionalPanel(
          condition = "input.query_type == 'Sugar Range'",
          numericInput("min_sugar", "Min Sugar:", value = 0),
          numericInput("max_sugar", "Max Sugar:", value = 1000)
        ),
        
        # Numeric inputs for carbohydrates range query
        conditionalPanel(
          condition = "input.query_type == 'Carbohydrates Range'",
          numericInput("min_carbohydrates", "Min Carbohydrates:", value = 0),
          numericInput("max_carbohydrates", "Max Carbohydrates:", value = 1000)
        ),
        
        # Numeric inputs for protein range query
        conditionalPanel(
          condition = "input.query_type == 'Protein Range'",
          numericInput("min_protein", "Min Protein:", value = 0),
          numericInput("max_protein", "Max Protein:", value = 1000)
        ),
        actionButton("query_button", "Query API"), # Button to query the API
        downloadButton("download_data", "Download Data") # Button to download data
      ),

        
        # Button to initiate the API query
        actionButton("query_button", "Query API"),
        
        # Button to download the queried data
        downloadButton("download_data", "Download Data")
      ),
      
      # Conditional panel for the 'About' tab
      conditionalPanel(
        condition = "input.tabs == 'About'",
        plotOutput("calendarPlot")  # Display calendar plot
      ),
      # Conditional panel for the 'Data Exploration' tab
      conditionalPanel(
        condition = "input.tabs == 'Data Exploration'",
        
        # Checkbox group input for selecting plots to display
        checkboxGroupInput("plots", "Select Plots to Display:", 
                           choices = c("Used Data" = "datatab",
                                       "Summary Stats Table" = "contingency",
                                       "Bar Plot" = "bar", 
                                       "Box Plot" = "box", 
                                       "Correlation Plot" = "corr",
                                       "Heatmap" = "heatmap"
                           )),
        uiOutput("familyInput"), # Dynamic UI for family selection
        #actionButton("plot_button", "Generate Plots")# Button to generate plots

        # Dropdown menu for selecting fruit family
        selectInput("family", "Select Family:", choices = c("All", unique(data$family))),
        
        # Button to generate plots
        actionButton("plot_button", "Generate Plots")

      )
    ),
    
    # Main panel to display the content based on the selected tab
    mainPanel(
      tabsetPanel(
        id = "tabs", # Tabset panel for different views
        
        # 'About' tab content
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
        
        # 'Data Download' tab content
        tabPanel("Data Download", 
                 DTOutput("data_table") # Display data table
        ),
        
        # 'Data Exploration' tab content
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
