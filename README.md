# Project 2

### 1. Brief description of the app and its purpose.

This Shiny app provides an analysis of nutritional data for various fruits. It allows users to download data, filter it based on specific criteria, and visualize the data using various plots. The data is sourced from the Fruityvice API, and the app includes functionalities to query this API based on different parameters such as family, order, genus, and nutritional ranges (calories, fat, sugar, carbohydrates, and protein). Additionally, the app includes a calendar plot and an "About" section that provides details about the app's purpose and data source.

### 2. A list of packages needed to run the app.

```         
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
```

### 3. A line of code that would install all the packages used.

```         
install.packages(c("shiny", "ggplot2", "ggcorrplot", "dplyr", "reshape2", "DT", "httr", "jsonlite", "calendR", "ggrepel"))
```

### 4. The shiny::runGitHub() code

```         
library(shiny) 
shiny::runGitHub(repo = "Project2", username = "whiterabbit077", subdir = "Project2")
```
