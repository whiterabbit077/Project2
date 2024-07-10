# server.R

# Load necessary libraries
library(shiny)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(reshape2)
library(DT)
library(httr)
library(jsonlite)
library(calendR)
library(ggrepel)

# Define functions to query the Fruityvice API

# Function to get all fruits data
get_all_fruits <- function() {
  url <- "https://www.fruityvice.com/api/fruit/all"
  response <- httr::GET(url)
  if (httr::status_code(response) == 200) {
    data <- fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    return(data)
  } else {
    return(NULL)
  }
}

# Function to get fruits data by family name
get_family_by_name <- function(family) {
  url <- paste0("https://www.fruityvice.com/api/fruit/family/", family)
  response <- httr::GET(url)
  if (httr::status_code(response) == 200) {
    data <- fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    return(data)
  } else {
    return(NULL)
  }
}

# Function to get fruits data by order name
get_order_by_name <- function(order) {
  url <- paste0("https://www.fruityvice.com/api/fruit/order/", order)
  response <- httr::GET(url)
  if (httr::status_code(response) == 200) {
    data <- fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    return(data)
  } else {
    return(NULL)
  }
}

# Function to get fruits data by genus name
get_genus_by_name <- function(genus) {
  url <- paste0("https://www.fruityvice.com/api/fruit/genus/", genus)
  response <- httr::GET(url)
  if (httr::status_code(response) == 200) {
    data <- fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    return(data)
  } else {
    return(NULL)
  }
}

# Function to get fruits data by calories range
get_fruits_by_calories <- function(min_calories, max_calories) {
  url <- paste0("https://www.fruityvice.com/api/fruit/calories?min=", min_calories, "&max=", max_calories)
  response <- httr::GET(url)
  if (httr::status_code(response) == 200) {
    data <- fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    return(data)
  } else {
    return(NULL)
  }
}

# Function to get fruits data by fat range
get_fruits_by_fat <- function(min_fat, max_fat) {
  url <- paste0("https://www.fruityvice.com/api/fruit/fat?min=", min_fat, "&max=", max_fat)
  response <- httr::GET(url)
  if (httr::status_code(response) == 200) {
    data <- fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    return(data)
  } else {
    return(NULL)
  }
}

# Function to get fruits data by sugar range
get_fruits_by_sugar <- function(min_sugar, max_sugar) {
  url <- paste0("https://www.fruityvice.com/api/fruit/sugar?min=", min_sugar, "&max=", max_sugar)
  response <- httr::GET(url)
  if (httr::status_code(response) == 200) {
    data <- fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    return(data)
  } else {
    return(NULL)
  }
}

# Function to get fruits data by carbohydrates range
get_fruits_by_carbohydrates <- function(min_carbohydrates, max_carbohydrates) {
  url <- paste0("https://www.fruityvice.com/api/fruit/carbohydrates?min=", min_carbohydrates, "&max=", max_carbohydrates)
  response <- httr::GET(url)
  if (httr::status_code(response) == 200) {
    data <- fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    return(data)
  } else {
    return(NULL)
  }
}

# Function to get fruits data by protein range
get_fruits_by_protein <- function(min_protein, max_protein) {
  url <- paste0("https://www.fruityvice.com/api/fruit/protein?min=", min_protein, "&max=", max_protein)
  response <- httr::GET(url)
  if (httr::status_code(response) == 200) {
    data <- fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    return(data)
  } else {
    return(NULL)
  }
}

# Define the server logic for the Shiny app
shinyServer(function(input, output, session) {

  # Reactive value to store the fruit data
  fruit_data <- reactiveVal()
  
  # Observe the query button and fetch data from the API based on the query type

  observeEvent(input$query_button, {
    query_type <- input$query_type
    query_param <- input$query_param
    data_df <- NULL
    

    # Perform the query based on the selected query type
    if (query_type == "All Fruits") {
      data_df <- get_all_fruits()
    } else if (query_type == "Family by Name") {
      data_df <- get_family_by_name(query_param)
    } else if (query_type == "Order by Name") {
      data_df <- get_order_by_name(query_param)
    } else if (query_type == "Genus by Name") {
      data_df <- get_genus_by_name(query_param)
    } else if (query_type == "Calories Range") {
      data_df <- get_fruits_by_calories(input$min_calories, input$max_calories)
    } else if (query_type == "Fat Range") {
      data_df <- get_fruits_by_fat(input$min_fat, input$max_fat)
    } else if (query_type == "Sugar Range") {
      data_df <- get_fruits_by_sugar(input$min_sugar, input$max_sugar)
    } else if (query_type == "Carbohydrates Range") {
      data_df <- get_fruits_by_carbohydrates(input$min_carbohydrates, input$max_carbohydrates)
    } else if (query_type == "Protein Range") {
      data_df <- get_fruits_by_protein(input$min_protein, input$max_protein)
    }
    

    # If data is successfully retrieved, unnest the nutrition data and store it
    if (!is.null(data_df)) {
      # Unnest the nutrition data from the API response
      data_df <- data_df %>%
        unnest_wider(nutritions)
      fruit_data(data_df)
    } else {
      # Show an error modal if data retrieval fails
      showModal(modalDialog(
        title = "Error",
        "Failed to retrieve data."
      ))
    }
  })
  
  # Render the data table
  output$data_table <- renderDT({
    datatable(fruit_data())
  })
  
  # Define the download handler for the data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("fruit_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(fruit_data(), file, row.names = FALSE)
    }
  )
  
  # Filter the data based on the selected family
  data_filtered <- reactive({
    if (input$family == "All") {
      fruit_data()
    } else {
      fruit_data() %>% filter(family == input$family)
    }
  })
  
  # Render the UI for selected plots
  output$selectedPlots <- renderUI({
    input$plot_button
    isolate({
      plots <- input$plots
      plotOutputs <- list()
      
      # Add data table plot
      if ("datatab" %in% plots) {
        plotOutputs <- c(plotOutputs, list(h4("Selected Data Table"),
                                           DTOutput("dataTable"),
                                           br()))
      }
      # Add contingency table plot
      if ("contingency" %in% plots) {
        plotOutputs <- c(plotOutputs, list(h4("Statistics for Selected Data"),
                                           tableOutput("contingencyTable"),
                                           br()))
      }
      # Add bar plot
      if ("bar" %in% plots) {
        plotOutputs <- c(plotOutputs, list(h4("Bar Plot"),
                                           plotOutput("barPlot"),
                                           br()))
      }
      # Add box plot
      if ("box" %in% plots) {
        plotOutputs <- c(plotOutputs, list(h4("Box Plot"),
                                           plotOutput("boxPlot"),
                                           br()))
      }
      # Add correlation plot
      if ("corr" %in% plots) {
        plotOutputs <- c(plotOutputs, list(h4("Correlation Plot"),
                                           plotOutput("corrPlot"),
                                           br()))
      }
      # Add heatmap plot
      if ("heatmap" %in% plots) {
        plotOutputs <- c(plotOutputs, list(h4("Heatmap"),
                                           plotOutput("heatmapPlot"),
                                           br()))
      }
      plotOutputs
    })
  })
  
 # Render the data table for selected data
  output$dataTable <- renderDT({
    input$plot_button
    isolate({
      req("datatab" %in% input$plots)
      df <- data_filtered()
      datatable(df, options = list(pageLength = 10))
    })
  })
  
  # Render the contingency table
  output$contingencyTable <- renderTable({
    input$plot_button
    isolate({
      req("contingency" %in% input$plots)
      df <- data_filtered()
      df %>%
        select(-id) %>%
        summarise(across(where(is.numeric), list(
          mean = mean,
          median = median,
          sd = sd,
          min = min,
          max = max
        ))) %>%
        pivot_longer(cols = everything(), names_to = c("variable", "stat"), names_sep = "_") %>%
        pivot_wider(names_from = stat, values_from = value)
    })
  })
  
  # Render the bar plot
  output$barPlot <- renderPlot({
    input$plot_button
    isolate({
      req("bar" %in% input$plots)
      df <- data_filtered()
      melted_data <- melt(df, id.vars = c("name", "id", "family", "order", "genus"))
      nutrition_data <- melted_data[melted_data$variable %in% c("calories", "fat", "sugar", "carbohydrates", "protein"),]
      
      ggplot(nutrition_data,
             aes(x = name, y = value, fill = variable)) +
        geom_bar(stat = "identity") +
        labs(title = "Nutritional Composition of Selected Fruits", x = "Fruit", y = "Nutritional Value") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 19, face = "bold"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14)
        ) +
        coord_flip()
    })
  })
  
  # Render the box plot
  output$boxPlot <- renderPlot({
    input$plot_button
    isolate({
      req("box" %in% input$plots)
      df <- data_filtered()
      
      if (nrow(df) == 0) {
        showModal(modalDialog(
          title = "Error",
          "No data available to generate the box plot."
        ))
        return(NULL)
      }
      
      melted_data <- tryCatch({
        melt(df, id.vars = c("name", "id", "family", "order", "genus"))
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Error in melting data:", e$message)
        ))
        return(NULL)
      })
      
      if (is.null(melted_data)) return(NULL)
      
      nutrition_data <- melted_data[melted_data$variable %in% c("calories", "fat", "sugar", "carbohydrates", "protein"),]
      
      p <- tryCatch({
        ggplot(nutrition_data, aes(x = variable, y = value, fill = variable)) +
          geom_boxplot(outlier.shape = NA) + 
          geom_jitter(width = 0.2, height = 0) + 
          labs(title = "Distribution of Nutritional Values Across Selected Fruits", x = "Nutritional Element", y = "Value") +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 19, face = "bold"),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14)
          )
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Error in creating plot:", e$message)
        ))
        return(NULL)
      })
      
      if (is.null(p)) return(NULL)
      
      outliers <- tryCatch({
        nutrition_data %>%
          group_by(variable) %>%
          mutate(
            Q1 = quantile(value, 0.25, na.rm = TRUE),
            Q3 = quantile(value, 0.75, na.rm = TRUE),
            IQR = IQR(value, na.rm = TRUE),
            is_outlier = value > (Q3 + 1.5 * IQR) | value < (Q1 - 1.5 * IQR)
          ) %>%
          filter(is_outlier)
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Error in calculating outliers:", e$message)
        ))
        return(NULL)
      })

      if (nrow(outliers) > 0) {
        p <- p + geom_text_repel(data = outliers, aes(label = name), vjust = -0.5, size = 4, color = "red", max.overlaps = Inf)
      }
      
      print(p)
    })
  })
  
  # Render the correlation plot
  output$corrPlot <- renderPlot({
    input$plot_button
    isolate({
      req("corr" %in% input$plots)
      df <- data_filtered()
      if (nrow(df) > 1) {
        numeric_data <- df %>% select(calories, fat, sugar, carbohydrates, protein)
        corr_matrix <- cor(numeric_data, use = "complete.obs")
        
        ggcorrplot(corr_matrix, method = "circle") +
          labs(title = "Nutritional Values Across Selected Fruits") +
          theme(
            plot.title = element_text(size = 19, face = "bold"),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14)
          )
      } else {
        output$warningText <- renderText({
          "Not enough data to generate correlation plot for this family."
        })
      }
    })
  })
  
  # Render the heatmap plot
  output$heatmapPlot <- renderPlot({
    input$plot_button
    isolate({
      req("heatmap" %in% input$plots)
      df <- data_filtered()
      melted_data <- melt(df, id.vars = c("name", "id", "family", "order", "genus"))
      nutrition_data <- melted_data[melted_data$variable %in% c("calories", "fat", "sugar", "carbohydrates", "protein"),]
      
      ggplot(nutrition_data,
             aes(x = variable, y = name, fill = value)) +
        geom_tile() +
        labs(title = "Heatmap of Nutritional Values Across Fruits", x = "Fruit", y = "Nutritional Element") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 19, face = "bold"),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14)
        )
    })
  })
  
 # Render the calendar plot
  output$calendarPlot <- renderPlot({
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    current_month <- as.numeric(format(Sys.Date(), "%m"))
    current_day <- as.numeric(format(Sys.Date(), "%d"))
    
    calendR(
      year = current_year,
      month = current_month,
      special.days = current_day,
      special.col = "lightblue",
      start = "M"
    )
  })
  
  # Dynamic UI for family selection
  output$familyInput <- renderUI({
    selectInput("family", "Select Family:", choices = c("All", unique(fruit_data()$family)))
  })
})
