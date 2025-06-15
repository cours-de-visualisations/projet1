library(shiny)
library(tidyverse)
library(DT)
library(lubridate)
library(stringr)
library(tools)
library(leaflet)
library(shinythemes)


load("data/kc_house_data.RData")
# On met les dates au bon format pour les séries temporelles
if (!inherits(house$date, "Date")) {
  house <- house %>%
    mutate(
      date = ymd(substr(date, 1, 8))  
    )
}

# On ajoute la colonne 'region',  pour pouvoir selectionner selon les localités
house <- house %>%
  mutate(region = case_when(
    lat >= median(lat) & long < median(long) ~ "Nord Ouest",
    lat >= median(lat) & long >= median(long) ~ "Nord Est",
    lat < median(lat) & long < median(long) ~ "Sud Ouest",
    lat < median(lat) & long >= median(long) ~ "Sud Est",
    TRUE ~ "Autre"
  ))


ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
 #### j'ajoute une image en arrière plan 
 #### Pour ajouter l'arriere plan, on s'est fait aider par ChatGpt
 tags$head(
   tags$style(HTML("
    body {
      background-image: url('logo1.png');
      background-size: cover;
      background-attachment: fixed;
      background-position: center;
      background-repeat: no-repeat;
      position: relative;
    }
    
    body::before {
      content: '';
      position: fixed;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      background-color: rgba(0, 0, 0, 0.4); /* assombrit le fond */
      z-index: -1;
    }

    .well, .tab-content, .dataTables_wrapper, .form-group {
      background-color: rgba(255, 255, 255, 0.9);
      padding: 10px;
      border-radius: 10px;
    }
  "))
 ),
  
  h1(tags$b("Gestion immobilière"), style = "text-align: center;color: white;"),
  br(), br(),
  
  sidebarLayout(
    sidebarPanel(
      h4("Profil", style = "text-align: center;"),
      
      div(style = "display: flex; gap: 10px; justify-content: center;",
          actionButton("acheteur", "Acheteur"),
          actionButton("proprietaire", "Propriétaire")
      ),
      
      br(), br(), br(),
      
      checkboxGroupInput(inputId = "Localité", 
                         label = "Localité",
                         choices = c("Nord Ouest", "Nord Est", "Sud Ouest", "Sud Est"),
                         selected = "Nord Ouest")
  
    ),
    
    
    mainPanel(
########################################################### Les graphiques
      tabsetPanel(type = "tabs",
                  tabPanel("Graphiques",
                           selectInput("graphiques","graphiques",
                                       choices =  c("distribution des prix" = "distributionPrix",
                                                    "nuage des points"="nuagePoints",
                                                    "nombre de chambre par maison"="nombre_chambre_par_maison",
                                                    "grade maison"="grade_maison", 
                                                    "vue sur l'eau"="vue_sur_eau", 
                                                    "Etat des maisons"="etat_maison",
                                                    "tendance des prix"="tendancePrix")),
                           conditionalPanel("input.graphiques == 'distributionPrix'", plotOutput("distributionPrix")),
                           conditionalPanel("input.graphiques == 'nuagePoints'", plotOutput("nuagePoints"),
                                            selectInput("x", "Variable explicative :", 
                                                        choices = c("Nombre de chambres" = "bedrooms",
                                                                    "Nombre de salles de bain" = "bathrooms",
                                                                    "Surface habitable" = "sqft_living",
                                                                    "Taille du terrain" = "sqft_lot",
                                                                    "Surface au dessus du sol" = "sqft_above"),
                                                        selected = "sqft_living"),
                                            sliderInput("alpha", "Alpha (transparence) :", min = 0, max = 1, value = 0.5)),
                           
                           conditionalPanel("input.graphiques == 'nombre_chambre_par_maison'", plotOutput("nombre_chambre_par_maison")),
                           conditionalPanel("input.graphiques == 'grade_maison'", plotOutput("grade_maison")),
                           conditionalPanel("input.graphiques == 'vue_sur_eau'", plotOutput("vue_sur_eau")),
                           conditionalPanel("input.graphiques == 'etat_maison'", plotOutput("etat_maison")),
                           conditionalPanel("input.graphiques == 'tendancePrix'", plotOutput("tendancePrix"))
                  ),
                  
                  
 ############################################################# Les resumés                  
                  
                  tabPanel("Statistiques globales ",
                           h3(tags$b("Statistiques par régions")),tableOutput("tableau_secteurs"), br(),
                           h3(tags$b("Évolution des prix dans le temps")), plotOutput("tendance_globales_Prix")
                           ), 
 
                 tabPanel("Carte et données",
                          leafletOutput("carte", height = 500),
                           br(),
                          ),  
 
                  tabPanel("Références",
                           br(),
                           
                           
                           h4(tags$b("À propos des données")),
                           p("Notre travail s’est basé sur un ensemble de données provenant du comté de King, 
                             dans l’État de Washington, aux États-Unis. Toutefois, 
                             cette analyse pourrait être généralisée à n’importe quelle région du monde. Tant 
                             qu'on a tous les éléments necessaires"), 
                           br(),
                           
                           tags$a("Les données ont été extraites ici ",
                                  href = "https://www.kaggle.com/datasets/harlfoxem/housesalesprediction"), "👈",
                           br(), br(),
                           
                           h4(tags$b("brève description des variables manipulées :")),
                           
                
                           tags$ul(
                             tags$li(tags$b("prix"), ": prix de vente de la maison"),
                             tags$li(tags$b("nombre de chambres"), ": nombre de chambres dans une maison mise en vente"),
                             tags$li(tags$b("nombre de salles de bain"), ": nombre de salles de bain dans une maison mise en vete"),
                             tags$li(tags$b("surface habitable"), ": surface habitable (en pieds carrés)"),
                             tags$li(tags$b("taille du terrain"), ": taille du terrain (en pieds carrés)"),
                           )
                  )
      ),
      br(),
      div(style = "color: white; font-weight: bold;", textOutput("profil_selectionne"))
      
    )
  )
)

server <- function(input, output) {
  
  profil <- reactiveVal("Aucun profil sélectionné ")
  
  observeEvent(input$acheteur, {
    profil("Profil: Acheteur")
  })
  
  observeEvent(input$proprietaire, {
    profil("Profil: Propriétaire")
  })
  
  output$profil_selectionne <- renderText({
    profil()
  })
  
 
  
  secteur <- reactive({
    req(input$Localité)
    filter(house, region %in% input$Localité)
  })
  
  ################################################# Les graphiques##########################################
  
  ######################################## G1
  
  output$distributionPrix <- renderPlot({
    ggplot(secteur(), aes(x = price / 1000)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 50) +
      labs(
        title = "G1 : Distribution des prix des maisons",
        x = "Prix (en milliers USD)",
        y = "Nombre de maisons"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })
  
  ######################################## G 2
  prix <- reactive({
    secteur() %>%
      mutate(price = price/1000000)})
  
  output$nuagePoints <- renderPlot({
    variables_francais <- c(
      bedrooms = "Nombre de chambres",
      bathrooms = "Nombre de salles de bain",
      sqft_living = "Surface habitable",
      sqft_lot = "Taille du terrain",
      sqft_above = "Surface au-dessus du sol"
    )
    ggplot(prix(), aes_string(x = input$x, y = "price", color = "factor(view)")) +
      geom_jitter(alpha = input$alpha) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(
        title = "G2 : répartition  des prix des maisons",
        x = variables_francais[[input$x]],
        y = "Prix (en million USD)",
        color = "Vue"
      ) +
      scale_color_manual(
        values = c("0" = "gray50", "1" = "gold", "2" = "darkorange", 
                   "3" = "royalblue", "4" = "forestgreen"),
        labels = c("aucune vue", "vue faible", "vue moyenne", "bonne vue", "vue excellente")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })
  
  
  ######################################### G3
  calcul_tendances <- reactive({
    secteur() %>%
      mutate(jour = floor_date(date, unit = "day")) %>%
      group_by(jour) %>%
      summarise(prix_moyen = mean(price) / 1000) %>%
      ungroup()
  })
    
  output$tendancePrix <- renderPlot({
    ggplot(calcul_tendances(), aes(x = jour, y = prix_moyen)) +
      geom_line(color = "skyblue", linewidth = 1) + 
      labs(title = "G3 : Tendance des prix moyens dans le temps",
           x = NULL,
           y = "Prix moyen (en milliers de $)") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16,face = "bold"),
        axis.text.x = element_text(angle = 60, hjust = 1) 
      ) +
      scale_x_date(date_breaks = "2 month", date_labels = "%b %Y")
  })
  
  ######################################### G 4
  calcul_nombre_maison <- reactive({
    secteur() %>%
      group_by(bedrooms) %>%
      summarise(nombre_de_maisons = n()) %>%
      arrange(bedrooms)
  })
  
  output$nombre_chambre_par_maison <- renderPlot({
    ggplot(calcul_nombre_maison(), aes(x = factor(bedrooms), y = nombre_de_maisons)) +
      geom_col(fill = "skyblue", color = "black") +
      labs(
        title = "G4 : Nombre de maisons par nombre de chambres",
        x = "Nombre de chambres",
        y = "Nombre de maisons"
      ) +
      theme_minimal()+
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })
  
######################################### D1
  
  resume_grade_maison <- reactive({
     secteur() %>%
      group_by(grade) %>%
      summarize(count = n()) %>%
      mutate(percentage = round((count / sum(count)) * 100, 1))
  })
  output$grade_maison<-renderPlot({
    ggplot(resume_grade_maison(), aes(x = factor(grade), y = count, fill = factor(grade))) +
      geom_bar(stat = "identity", color = "black", width = 0.7)  +  
      scale_fill_viridis_d(option = "D") +  
      theme_minimal() +
      labs(
        title = "D1 : Nombre de maisons selon la qualité",
        x = "Grade (Qualité de la construction)",
        y = "Nombre de maisons",
        fill = "Grade"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text.x = element_text( size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none"  
      )
  })

######################################### D2
compte_vue_sur_eau <- reactive({
    secteur()%>%
      group_by(waterfront) %>%
      summarize(count = n()) %>%
      mutate(percentage = round((count / sum(count)) * 100, 1))
  })
output$vue_sur_eau <- renderPlot({
  ggplot(compte_vue_sur_eau(), aes(x = "", y = count, fill = factor(waterfront))) +  
    geom_bar(stat = "identity") +  
    coord_polar(theta = "y") + 
    scale_fill_manual(
      values = c("0" = "gray", "1" = "royalblue"),
      labels = c("0" = "non", "1" = "oui")
    ) + 
    labs(
      title = "D2 : Vue sur l'eau",
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(), 
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = "right"
    )
})

######################################### D3
compte_etat_maison <- reactive({
  secteur() %>%
    group_by(condition) %>%
    summarize(count = n()) %>%
    mutate(percentage = round((count / sum(count)) * 100, 1)) 
})
output$etat_maison <- renderPlot({
  ggplot(compte_etat_maison(), aes(x = factor(condition), y = count)) +
    geom_bar(stat = "identity", color = "gray30", width = 0.7) +
    geom_text(aes(label = paste0(" (", percentage, "%)")), 
              vjust = -0.5, size = 3.5, color = "black") + 
    scale_colour_viridis_d() +
    theme_minimal() +
    labs(
      title = "D3 : Etat général des maisons",
      x = NULL,
      y = "Nombre de maisons",
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = "none"  
    )+
    scale_x_discrete(labels=c("mauvaise", "acceptable", "bonne", "très bonne", "excellente"))
  
})

################################################# resumé et statistiques globales ##########################################
######################################### Nombre de maisons par secteur
nombre_maisons_par_secteur <- reactive({
  house %>%
    group_by(region) %>%
    summarise("nombre de maisons" = n(),
              "prix moyen" = round(mean(price), 2),
              "prix median" = round(median(price), 2),
              "prix min" = min(price),
              "prix max" = max(price),
              "surface moyenne" = round(mean(sqft_living), 2)) %>%
    arrange(region)
})

output$tableau_secteurs <- renderTable({
  nombre_maisons_par_secteur()
})

####################################### Tendance globale des prix
calcul_tendances_globales <- house %>%
    mutate(jour = floor_date(date, unit = "day")) %>%
    group_by(jour) %>%
    summarise(prix_moyen = mean(price) / 1000) %>%
    ungroup()

output$tendance_globales_Prix <- renderPlot({
  ggplot(calcul_tendances_globales, aes(x = jour, y = prix_moyen)) +
    geom_line(color = "skyblue", linewidth = 1) + 
    labs(title = NULL,
         x = NULL,
         y = "Prix moyen (en milliers de $)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16,face = "bold"),
      axis.text.x = element_text(angle = 60, hjust = 1) 
    ) +
    scale_x_date(date_breaks = "2 month", date_labels = "%b %Y")
})

  
################################################################ visualisation sur la carte ##############################
################################################################ 

output$carte <- renderLeaflet({
  leaflet(secteur()) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~long,
      lat = ~lat,
      popup = ~paste("Prix :", price, "USD"),
      color = "blue",
      radius = 0.0001
    )
})


}

shinyApp(ui = ui, server = server)
