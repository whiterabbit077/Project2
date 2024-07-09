#------------------- Install packages if not already installed
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("ggcorrplot")
# install.packages("reshape2")
# install.packages("dplyr")

#------------------- Load libraries
library(httr)
library(jsonlite)
library(ggplot2)
library(ggcorrplot)
library(reshape2)
library(dplyr)

#-------------------- Define the API endpoint
url <- "https://www.fruityvice.com/api/fruit/all"

# Make the GET request
response <- httr::GET(url)

# Check the status code of the response
if (status_code(response) == 200) {
  # Parse the JSON content
  data <- fromJSON(content(response, "text"))
  print(data)
  str(data)
} else {
  print(paste("Error:", status_code(response)))
}

# Flatten the nested structure of nutritions column
data <- data |>
  unnest_wider(nutritions)

#remove Huzelnut
data <- data %>% 
  filter(name != "Hazelnut")

###-----------------------PLOTS:
############ plot 1: Nutritional Composition of Fruits
# Melt the data for easier plotting with stacked bars
melted_data <- melt(data, id.vars = c("name", "id", "family", "order", "genus"))

# Plot
ggplot(melted_data[melted_data$variable %in% c("calories", "fat", "sugar", "carbohydrates", "protein"),], 
       aes(x = name, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Nutritional Composition of Fruits", x = "Fruit", y = "Nutritional Value") +
  theme_minimal() +
  coord_flip() 

############### plot 2: Distribution of Nutritional Values Across Fruits
# Assuming melted_data is the melted data frame
ggplot(melted_data[melted_data$variable %in% c("calories", "fat", "sugar", "carbohydrates", "protein"),], 
       aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Distribution of Nutritional Values Across Fruits", x = "Nutritional Element", y = "Value") +
  theme_minimal()

################# Corr plot
# Select relevant numeric columns
numeric_data <- data %>% 
  select(calories, fat, sugar, carbohydrates, protein)
# Calculate the correlation matrix
corr_matrix <- cor(numeric_data, use = "complete.obs")

# Create the correlation plot
ggcorrplot(corr_matrix, method = "circle")#, lab = TRUE)




