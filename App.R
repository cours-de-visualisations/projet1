library(shiny)
library(tidyverse)
library(DT)
library(stringr)
library(tools)

# Chargement des données
load("data/kc_house_data.RData")  # Assure-toi que ce fichier contient un objet nommé `house`

ui <- fluidPage(
  titlePanel("Gestion immobilière"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("y", "Y-axis:", 
                  choices = c("price", "bedrooms", "bathrooms", "sqft_living", 
                              "sqft_lot", "floors", "sqft_above", "lat", "long"), 
                  selected = "price"),
      
      selectInput("x", "X-axis:", 
                  choices = c("price", "bedrooms", "bathrooms", "sqft_living", 
                              "sqft_lot", "floors", "sqft_above", "lat", "long"), 
                  selected = "bedrooms"),
      
      selectInput("z", "Color by:", 
                  choices = c("price", "bedrooms", "bathrooms", "sqft_living", 
                              "sqft_lot", "floors", "sqft_above", "lat", "long"), 
                  selected = "floors"),
      
      sliderInput("alpha", "Alpha:", min = 0, max = 1, value = 0.5),
      sliderInput("size", "Size:", min = 0, max = 5, value = 2)
    ),
    
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)

server <- function(input, output) {
  output$scatterplot <- renderPlot({
    ggplot(house, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(
        x = toTitleCase(str_replace_all(input$x, "_", " ")),
        y = toTitleCase(str_replace_all(input$y, "_", " ")),
        color = toTitleCase(str_replace_all(input$z, "_", " "))
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
