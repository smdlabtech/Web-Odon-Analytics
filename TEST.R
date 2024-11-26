#******** Loading Necessary Packages ********
library(shiny)
library(shinydashboard)
library(bslib)
library(reactable)
library(plotly)
library(leaflet)
library(dplyr)
library(readr)
library(lubridate)

#******** Helper Function to Load Files Safely ********
load_odon_file <- function(file_path) {
  if (file.exists(file_path)) {
    # Lecture des fichiers Odon avec des colonnes prédéfinies
    data <- read_csv(file_path, skip = 1, col_names = TRUE) %>%
      setNames(c("Id", "Date", "Heure_GMT_plus2h", "Temp")) %>%
      mutate(
        Id = as.integer(Id),                  # Conversion en entier
        Date = dmy(Date),                    # Conversion en date
        Heure_GMT_plus2h = hms(Heure_GMT_plus2h), # Conversion en temps
        Temp = as.numeric(Temp)              # Conversion en double
      )
    return(data)
  } else {
    warning(paste("Le fichier", file_path, "n'existe pas."))
    return(NULL)
  }
}

load_table <- function(file_path, col_types) {
  if (file.exists(file_path)) {
    read_csv(file_path, col_types = col_types)
  } else {
    warning(paste("Le fichier", file_path, "n'existe pas."))
    return(NULL)
  }
}

#******** Define UI ********
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Dashboard Amélioré"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tables", tabName = "tables", icon = icon("table")),
      menuItem("Visualisations", tabName = "visualizations", icon = icon("chart-line")),
      menuItem("Résumé Statistiques", tabName = "stats_summary", icon = icon("list-alt")),
      menuItem("Carte Interactive", tabName = "map_view", icon = icon("map"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.jsdelivr.net/npm/bootstrap@4.5.2/dist/css/bootstrap.min.css")),
    tabItems(
      # Onglet Tables
      tabItem(
        tabName = "tables",
        fluidRow(
          box(
            width = 12,
            title = "Tables Interactives",
            tabsetPanel(
              tabPanel("Base BDD", reactableOutput("table_base_bdd")),
              tabPanel("Coordonnées Géographiques", reactableOutput("table_coord_geo")),
              tabPanel("Coordonnées", reactableOutput("table_coordonnees")),
              tabPanel("Réseau Température", reactableOutput("table_reseau")),
              tabPanel("OdonT1 812", reactableOutput("table_812")),
              tabPanel("OdonT2 813", reactableOutput("table_813")),
              tabPanel("OdonT4 815", reactableOutput("table_815")),
              tabPanel("OdonT5 816", reactableOutput("table_816"))
            )
          )
        )
      ),
      # Onglet Visualisations
      tabItem(
        tabName = "visualizations",
        fluidRow(
          box(
            width = 4,
            title = "Options de Visualisation",
            selectInput("selected_file", "Fichier", choices = c("OdonT1 812", "OdonT2 813", "OdonT4 815", "OdonT5 816")),
            dateRangeInput("date_range", "Plage de Dates", start = NULL, end = NULL)
          ),
          box(
            width = 8,
            title = "Graphique Interactif",
            plotlyOutput("interactive_plot")
          )
        )
      ),
      # Onglet Résumé Statistique
      tabItem(
        tabName = "stats_summary",
        fluidRow(
          box(
            width = 12,
            title = "Résumé Statistiques",
            reactableOutput("summary_table")
          )
        )
      ),
      # Onglet Carte Interactive
      tabItem(
        tabName = "map_view",
        fluidRow(
          box(
            width = 4,
            title = "Options de la Carte",
            selectInput("probe", "Sélectionner une Sonde", choices = NULL),
            checkboxInput("show_markers", "Afficher les Marqueurs", TRUE)
          ),
          box(
            width = 8,
            title = "Carte Interactive",
            leafletOutput("map_view")
          )
        )
      )
    )
  )
)

#******** Define Server ********
server <- function(input, output, session) {
  # Répertoire contenant les fichiers
  data_dir <- "_data"
  
  # Chargement des fichiers
  base_bdd <- load_table(file.path(data_dir, "base_bdd.csv"), cols(
    date = col_date(format = "%d/%m/%Y"),
    TempOd1 = col_double(),
    TempOd2 = col_double(),
    TempOd4 = col_double(),
    TempOd5 = col_double()
  ))
  
  coord_geo <- load_table(file.path(data_dir, "coord_geo.csv"), cols(
    id_sonde = col_integer(),
    lib_sonde = col_character(),
    lat_wgs84 = col_double(),
    lon_wgs84 = col_double(),
    x_lamb93 = col_double(),
    y_lamb93 = col_double()
  ))
  
  coordonnees <- load_table(file.path(data_dir, "coordonnees.csv"), cols(
    id_sonde = col_integer(),
    lib_sonde = col_character(),
    lat_wgs84 = col_double(),
    lon_wgs84 = col_double(),
    x_lamb93 = col_double(),
    y_lamb93 = col_double()
  ))
  
  reseau_temp <- load_table(file.path(data_dir, "l-reseautemprivieres-d-r28.csv"), cols(
    id_sonde = col_integer(),
    lib_sonde = col_character(),
    TopoOH = col_character(),
    CdOH = col_character(),
    reseau_perenne = col_integer(),
    lat_wgs84 = col_double(),
    lon_wgs84 = col_double(),
    x_lamb93 = col_double(),
    y_lamb93 = col_double()
  ))
  
  odon_812 <- load_odon_file(file.path(data_dir, "OdonT1 812_2011-2017.csv"))
  odon_813 <- load_odon_file(file.path(data_dir, "OdonT2 813_2011-2017.csv"))
  odon_815 <- load_odon_file(file.path(data_dir, "OdonT4 815_2011-2017.csv"))
  odon_816 <- load_odon_file(file.path(data_dir, "OdonT5 816_2011-2017.csv"))
  
  # Rendu des Tables
  output$table_base_bdd <- renderReactable({ reactable(base_bdd) })
  output$table_coord_geo <- renderReactable({ reactable(coord_geo) })
  output$table_coordonnees <- renderReactable({ reactable(coordonnees) })
  output$table_reseau <- renderReactable({ reactable(reseau_temp) })
  output$table_812 <- renderReactable({ reactable(odon_812) })
  output$table_813 <- renderReactable({ reactable(odon_813) })
  output$table_815 <- renderReactable({ reactable(odon_815) })
  output$table_816 <- renderReactable({ reactable(odon_816) })
  
  # Visualisation Interactive
  selected_data <- reactive({
    req(input$selected_file)
    switch(input$selected_file,
           "OdonT1 812" = odon_812,
           "OdonT2 813" = odon_813,
           "OdonT4 815" = odon_815,
           "OdonT5 816" = odon_816)
  })
  
  output$interactive_plot <- renderPlotly({
    req(selected_data(), input$date_range)
    data_filtered <- selected_data() %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    plot_ly(data_filtered, x = ~Date, y = ~Temp, type = "scatter", mode = "lines+markers") %>%
      layout(
        title = paste("Température pour", input$selected_file),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Température (°C)"),
        hovermode = "x"
      )
  })
}

#******** Launch App ********
shinyApp(ui, server)
