library(shiny)
library(tidyverse)
library(DT)
library(stringr)
library(tools)
library(shinythemes)

load("data/kc_house_data.RData")

ui <- fluidPage(
  
  h1(tags$b("Gestion immobilière"), style = "text-align: center;"),
  br(), br(),
  
  sidebarLayout(
    sidebarPanel(
      
      h4("Profil",style = "text-align: center;"),
      
      div(style = "display: flex; gap: 10px; justify-content: center;",
          actionButton("btn_acheteur", "Acheteur"),
          actionButton("btn_proprietaire", "Propriétaire")
      ),
      
      br(), br(), br(),
      
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
      
      sliderInput("alpha", "Alpha (transparence):", min = 0, max = 1, value = 0.5),
      sliderInput("size", "Size (taille des points):", min = 0, max = 5, value = 2)
    ),
    
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Plot", plotOutput("scatterplot")),
        tabPanel("Summary", tableOutput("summary")),               
        tabPanel("Data", DT::dataTableOutput("data")),               
        tabPanel("Reference",
                 br(),
                 p("Voici une brève description des variables principales :"),
                 tags$ul(
                   tags$li(tags$b("price"), ": prix de vente de la maison"),
                   tags$li(tags$b("bedrooms"), ": nombre de chambres"),
                   tags$li(tags$b("bathrooms"), ": nombre de salles de bain"),
                   tags$li(tags$b("sqft_living"), ": surface habitable (en pieds carrés)"),
                   tags$li(tags$b("sqft_lot"), ": taille du terrain (en pieds carrés)"),
                   tags$li(tags$b("floors"), ": nombre d'étages"),
                   tags$li(tags$b("sqft_above"), ": surface au-dessus du sol"),
                   tags$li(tags$b("lat"), ": latitude"),
                   tags$li(tags$b("long"), ": longitude")
                 )
        )
      ),
      br(),
      textOutput("profil_selectionne")
    )  #fermeture du mainPanel
  )  #fermeture du sidebarLayout
)




server <- function(input, output) {
  
  profil <- reactiveVal("Aucun profil sélectionné")
  
  observeEvent(input$btn_acheteur, {
    profil("Profil: Acheteur")
  })
  
  observeEvent(input$btn_proprietaire, {
    profil("Profil: Propriétaire")
  })
  
  output$profil_selectionne <- renderText({
    profil()
  })
  
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
  
  output$summary <- renderTable({
    summary(house)
  })
  
  output$data <- renderDataTable({
    datatable(house, options = list(pageLength = 10))
  })
}




shinyApp(ui = ui, server = server)
