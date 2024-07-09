# server.R
library(shiny)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(reshape2)
library(DT)

shinyServer(function(input, output) {
  data <- reactiveVal()
  
  observeEvent(input$download_button, {
    response <- httr::GET(input$api_url)
    if (status_code(response) == 200) {
      data_df <- fromJSON(content(response, "text")) %>% 
        unnest_wider(nutritions) %>%
        filter(name != "Hazelnut")
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
  
  output$barPlot <- renderPlot({
    input$plot_button
    isolate({
      df <- data_filtered()
      melted_data <- melt(df, id.vars = c("name", "id", "family", "order", "genus"))
      nutrition_data <- melted_data[melted_data$variable %in% c("calories", "fat", "sugar", "carbohydrates", "protein"),]
      
      ggplot(nutrition_data, 
             aes(x = name, y = value, fill = variable)) +
        geom_bar(stat = "identity") +
        labs(title = "Nutritional Composition of Fruits", x = "Fruit", y = "Nutritional Value") +
        theme_minimal() +
        coord_flip() +
        facet_wrap(~ family, scales = "free_x")
    })
  })
  
  output$boxPlot <- renderPlot({
    input$plot_button
    isolate({
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
  
  output$warningText <- renderText({
    ""
  })
})
