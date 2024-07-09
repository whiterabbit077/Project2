# server.R
library(shiny)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(reshape2)
library(DT)
library(httr)
library(jsonlite)
library(calendR)

# Define functions to query the Fruityvice API
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

shinyServer(function(input, output) {
  data <- reactiveVal()
  
  observeEvent(input$query_button, {
    query_type <- input$query_type
    query_param <- input$query_param
    data_df <- NULL
    
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
    
    if (!is.null(data_df)) {
      data_df <- data_df %>%
        unnest_wider(nutritions) 
      data(data_df)
    } else {
      showModal(modalDialog(
        title = "Error",
        "Failed to retrieve data."
      ))
    }
  })
  
  output$data_table <- renderDT({
    datatable(data())
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("fruit_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  data_filtered <- reactive({
    if (input$family == "All") {
      data()
    } else {
      data() %>% filter(family == input$family)
    }
  })
  
  output$selectedPlots <- renderUI({
    input$plot_button
    isolate({
      plots <- input$plots
      plotOutputs <- list()
      
      if ("datatab" %in% plots) {
        plotOutputs <- c(plotOutputs, list(textOutput("Test"), DTOutput("dataTable")))
      }
      if ("contingency" %in% plots) {
        plotOutputs <- c(plotOutputs, list(tableOutput("contingencyTable")))
      }
      if ("bar" %in% plots) {
        plotOutputs <- c(plotOutputs, list(plotOutput("barPlot")))
      }
      if ("box" %in% plots) {
        plotOutputs <- c(plotOutputs, list(plotOutput("boxPlot")))
      }
      if ("corr" %in% plots) {
        plotOutputs <- c(plotOutputs, list(plotOutput("corrPlot")))
      }
      if ("heatmap" %in% plots) {
        plotOutputs <- c(plotOutputs, list(plotOutput("heatmapPlot")))
      }
      plotOutputs
    })
  })
  
  output$dataTable <- renderDT({
    input$plot_button
    isolate({
      req("datatab" %in% input$plots)
      df <- data_filtered()
      datatable(df, options = list(pageLength = 10))
    })
  })
  
  output$contingencyTable <- renderTable({
    input$plot_button
    isolate({
      req("contingency" %in% input$plots)
      df <- data_filtered()
      #table(df$family, df$order)
      #summarise(df)
      df |>
        summarise(across(where(is.numeric), list(
          mean = mean,
          median = median,
          sd = sd,
          min = min,
          max = max
        ))) |>
        pivot_longer(cols = everything(), names_to = c("variable", "stat"), names_sep = "_") |>
        pivot_wider(names_from = stat, values_from = value)
    })
  })
  
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
        labs(title = "Nutritional Composition of Fruits", x = "Fruit", y = "Nutritional Value") +
        theme_minimal() +
        coord_flip()
    })
  })
  
  output$boxPlot <- renderPlot({
    input$plot_button
    isolate({
      req("box" %in% input$plots)
      df <- data_filtered()
      melted_data <- melt(df, id.vars = c("name", "id", "family", "order", "genus"))
      nutrition_data <- melted_data[melted_data$variable %in% c("calories", "fat", "sugar", "carbohydrates", "protein"),]
      
      ggplot(nutrition_data, 
             aes(x = variable, y = value, fill = variable)) +
        geom_boxplot() +
        labs(title = "Distribution of Nutritional Values Across Fruits", x = "Nutritional Element", y = "Value") +
        theme_minimal()
    })
  })
  
  output$corrPlot <- renderPlot({
    input$plot_button
    isolate({
      req("corr" %in% input$plots)
      df <- data_filtered()
      if (nrow(df) > 1) {
        numeric_data <- df %>% select(calories, fat, sugar, carbohydrates, protein)
        corr_matrix <- cor(numeric_data, use = "complete.obs")
        
        ggcorrplot(corr_matrix, method = "circle", lab = TRUE)
      } else {
        output$warningText <- renderText({
          "Not enough data to generate correlation plot for this family."
        })
      }
    })
  })
  
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
        theme_minimal()
    })
  })
  
  
  
  output$calendarPlot <- renderPlot({
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    current_month <- as.numeric(format(Sys.Date(), "%m"))
    calendR(
      year = current_year, 
      month = current_month,
      start = "M"
    )
  })
})

