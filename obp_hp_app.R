#get wd
getwd()

# set working directory
setwd("C:/Users/zachary.bridger/Documents/hp_R/OBP_HP_Data")
# setwd("C:/Users/Owner/Documents/BASEBALL DATA")

# load packages
library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(shiny)

# Load data
obp_hp <- fread("hp_obp.csv")

# Sanitize column names to handle special characters
colnames(obp_hp) <- make.names(colnames(obp_hp), unique = TRUE)

# Define UI for application
ui <- navbarPage(
  "Open Biomechanics Project", theme = shinythemes::shinytheme("flatly"),
  
  tabPanel("High Performance Data",
           
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput(inputId = "LevelInput", label = "Select Playing Level(s)",
                                  choices = c("Pro", "College", "High School"),
                                  selected = c("Pro", "College", "High School")), 
               
               selectInput(inputId = "xInput", label = "Select X Metric:",
                           choices = colnames(obp_hp), selected = "peak_power_.w._mean_cmj"),
               
               selectInput(inputId = "yInput", label = "Select Y Metric:",
                           choices = colnames(obp_hp), selected = "pitch_speed_mph")
             ),
             
             mainPanel(
               fluidRow(
                 plotOutput("scatterplot"))
               )
             )
           )
  )


# Define server logic
server <- function(input, output, session) {
  
  # Filter data based on playing level
  dataFilter <- reactive({
    filteredData <- obp_hp %>% 
      filter(playing_level %in% input$LevelInput)
    return(filteredData)
  })
  
  # # Filter data based on playing level
  # dataFilter <- reactive({
  #   if (input$LevelInput != "All") {
  #     filteredData <- obp_hp %>% filter(playing_level == input$LevelInput)
  #   } else {
  #     filteredData <- obp_hp
  #   }
  #   return(filteredData)
  # })
  
  # Create the scatterplot
  output$scatterplot <- renderPlot({
    filteredData <- dataFilter()
    
    # Ensure the selected columns exist and are numeric
    x_var <- filteredData[[input$xInput]]
    y_var <- filteredData[[input$yInput]]
    
    # Filter out NA values
    filteredData <- filteredData %>%
      filter(!is.na(x_var) & !is.na(y_var) & x_var != 0 & y_var != 0)
    
    # Calculate the correlation coefficient
    corr_coef <- cor(x_var, y_var, use = "complete.obs")
    
    # Plot scatterplot
    ggplot(filteredData, aes_string(x = input$xInput, y = input$yInput)) + 
      geom_point(aes(color = playing_level)) +
      scale_color_manual(values = c("College" = "blue", "Pro" = "red", "High School" = "green")) +
      geom_smooth(method = "lm", se = TRUE) +
      labs(x = input$xInput, y = input$yInput, title = paste(input$yInput, "vs", input$xInput, 
                                                             sprintf(" | r = %.2f", corr_coef)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
