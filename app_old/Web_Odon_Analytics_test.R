#******** loading Packages ********
#https://statsandr.com/blog/anova-in-r/
  
library(RColorBrewer)
library(reactable)
library(shinyjs)
library(kableExtra)
library(ggrepel)
library(plotly)
library(IRdisplay)
library(stats)
library(data.table)
library(openxlsx)         #library to "ConvertToDate"
library(sp)
library(shiny)
library(leaflet)
library(DT)
library(tibble) 
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)
library(magrittr)
library(fpp)
library(sp)
#Application de formule dans les resumes statistiques

#****************************************************************************
#***************************** User Interface *******************************
# Definir l'interface Utilisateur pour l'app de telechargement de donnees----
ui <- shinyUI(
  
  #------ Page d'Acceuil du tableau de bord -------
  dashboardPage(skin="green",   #skin: pour changer la changer la couleur du tableau de bord
                
                ###A) Titre du tableau de bord
                dashboardHeader(title = "WO analytics"),
                
                ###B) Barre du Cote du Dashboard
                dashboardSidebar(tags$style(type = 'text/css',".badge{min-width: 200px;}"),
                                 
                                 sidebarMenu(
                                   # #--- Insertion du Menu sur la barre de cote droite ---
                                   # menuItem("Developpement en Cours !", tabName = 'dashboard')
                                   
                                 )),
                
                
                ###C) Corp de la navigation dans l'application
                dashboardBody(navbarPage(
                  titlePanel(""),
                  
                  # # Page1-------------------------------------------------------------------------------------------------------------------------------
                  tabPanel("Import data",icon = icon("database"),
                           
                           fluidRow(
                             sidebarLayout(#-- Panneau lateral pour les entrees
                               sidebarPanel(width = 3,
                                            
                                 fileInput("file1", "Upload data.  Choose CSV File",multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
                                 tags$hr(), #ligne horizontale
                                 tags$hr(),

                                 #Cocher le checkbox si le fihier a une entete
                                 checkboxInput("header", "Header", TRUE),
                                 radioButtons("sep", "Separator",choices = c(Comma = ","),selected = ","),
                                 radioButtons("quote", "Quote",choices = c("Double Quote" = '"'), selected = '"'),
                                
                                 tags$hr(),
                                 radioButtons("disp", "Display",choices = c(Head = "head",All = "all"),selected = "head")
                                #------------------------------------------------------------------------------------
                               ),
                               
                               #--- Paneau principal pour l'affichage des resultats ---
                               mainPanel(tableOutput("contents"),
                                         column(1),column(10, reactableOutput("table1")),column(1))
                             )
                           )
                           
                  ),
                  
                  # # Page2-------------------------------------------------------------------------------------------------------------------------------
                  tabPanel("Map view",icon = icon("database"),
                           #--- Affichage de la table de donnees en joli format HTML ---
                           fluidRow(
                             #-- Insertion zone Map --
                             leafletOutput("map"),
                             pickerInput("sonde", label = "Select a Probe:",
                                         choices = list("All probes", `Probes :` =c("Odon T1", "Odon T2", "Odon T4", "Odon T5")),
                                         options = list(`live-search` = TRUE)
                             ),
                             
                             #-- Insertion Map file --
                             sidebarLayout(
                               sidebarPanel(width = 3,fileInput("file2", "Upload data.  Choose CSV Map File",multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain", ".csv"))
                               ),
                               
                               mainPanel(tableOutput("Mapcontents"),
                                        column(1),column(10, reactableOutput("table2")),column(1)) #Affichage de la table de Choose CSV Data File
                                        
                             ) #Sidebarlayout
                             
                           ) #End Fluidrow
                           
                  ), #End tabPanel
                  
                  # # Page3-------------------------------------------------------------------------------------------------------------------------------
                  tabPanel("Interactive Overview",icon = icon("bar-chart-o"),
                           fluidPage(
                             
                             #------------------
                             fluidRow(column(1),
                                      box(width = 3,uiOutput('daterange')),
                                      box(width = 3,selectInput("Temp",label = "Select a probe:",c("TempOd1", "TempOd2", "TempOd4", "TempOd5")))
                              ),
                             
                             # absolutePanel(top = 130, left = 900, selectInput("Temp",label = "Select a probe:",c("TempOd1", "TempOd2", "TempOd4", "TempOd5"))),
                             
                             #------------------
                             fluidRow(column(1),
                               box(width=5,plotOutput('leplot6'),downloadButton('save_leplot6','')),
                               box(width=5,plotOutput('leplot7'),downloadButton('save_leplot7',''))
                             ) #End fluidRow
                             
                           )#End fluidPage
                           
                  ),#End tabPanel
                  
                  # # Page4-------------------------------------------------------------------------------------------------------------------------------
                  # column(width = 4,box(title = "Title 1", width = NULL, solidHeader = TRUE,collapsible = TRUE,status = "primary","Box content")),
                  tabPanel("Overview",icon = icon("bar-chart-o"),
                           fluidPage(
                             fluidRow(column(1),             
                               box(width=5,plotOutput('histplot1'),downloadButton('save_plot1','')),
                               box(width=5,plotOutput('histplot2'),downloadButton('save_plot2','')),
                             ),
                             
                             fluidRow(column(1), 
                               box(width=5,plotOutput('histplot4'),downloadButton('save_plot4','')),
                               box(width=5,plotOutput('histplot5'),downloadButton('save_plot5',''))
                             )
                             
                           )#End FluidPage
                           
                  ), #End tabPanel
                  
                  # # Page5-------------------------------------------------------------------------------------------------------------------------------
                  tabPanel("Statistics summary",icon = icon("list-alt"),
                           fluidPage(
                             
                             # Table de Resume Statistiques
                             fluidRow(
                               column(1),
                               column(11, box(width=6,reactableOutput("table4"))),
                               column(1)
                             )
                             
                           )#End FluidPage
                           
                  ) #End tabPanel

                )#End navbarPage 
                
                )#End dashboardBody
                
  ) #End dashboardPage
  
) #End shinyUI

#**************************************************************
#*************************** Server ***************************
# Definition de la logique du serveur pour lire le fichier ----
server<-function(input, output, session) {
  
  
  
  #-------------------------------------------------------- df1 -------------------------------------------------------------
  output$contents <- renderPrint({
    req(input$file1)
    tryCatch(
      { 
        df1 <- read.csv(input$file1$datapat,header = input$header,row.names = 1,encoding = 'UTF-8',sep = input$sep, quote = input$quote)
      },
      
      error = function(e) { #Gestion des erreurs
        stop(safeError(e)) 
      }
    )
    
    #--Sorties table de donnees--
    if(input$disp == "head") {
      resume_stats<-data.frame(matrix(df1 %>% summarise_each(funs(min,mean,median,max, sd), TempOd1,TempOd2, TempOd4,TempOd5),nrow = 4,ncol = 5))
       
      #Mise en forme (probe: "sonde")
      probe<-c('TempOd1','TempOd2','TempOd4','TempOd5')
      colnames(resume_stats)<-c('Min','Mean','Median','Max','Sd')
      resume_stats=cbind(probe,resume_stats)
      
      #Arrondir les valeurs de la table de resume statistiques
      resume_stats$Min=round(as.numeric(resume_stats$Min),2)
      resume_stats$Mean=round(as.numeric(resume_stats$Mean),2)
      resume_stats$Median=round(as.numeric(resume_stats$Median),2)
      resume_stats$Max=round(as.numeric(resume_stats$Max))
      resume_stats$Sd=round(as.numeric(resume_stats$Sd),2)
      #str(resume_stats)      

      #Affichage Resume de la table Statistique
      output$table4 <- renderReactable({
        reactable(resume_stats,
                  
                  #Mise en forme des colonnes
                  columns = list(
                    Min=colDef(format = colFormat(suffix = " C")),
                    Mean=colDef(format = colFormat(suffix = " C")),
                    Median=colDef(format = colFormat(suffix = " C")),
                    Max=colDef(format = colFormat(suffix = " C")),
                    Sd=colDef(format = colFormat(suffix = " C"))
                  ),
                  
                  )     
      })
      
      #Arrondir les valeurs de la table df1 de sortie
      df1$TempOd1=round(as.numeric(df1$TempOd1),0)
      df1$TempOd2=round(as.numeric(df1$TempOd2),2)
      df1$TempOd4=round(as.numeric(df1$TempOd4),2)
      df1$TempOd5=round(as.numeric(df1$TempOd5),2)
      
      output$table1 <- renderReactable({
        reactable(df1,compact = TRUE,
                  searchable = TRUE,defaultPageSize = 10,resizable = TRUE,
                  columns = list(
                    date = colDef(format = colFormat(date = TRUE, locales = "fr-FR")),
                    TempOd1=colDef(format = colFormat(suffix = " C")),
                    TempOd2=colDef(format = colFormat(suffix = " C")),
                    TempOd4=colDef(format = colFormat(suffix = " C")),
                    TempOd5=colDef(format = colFormat(suffix = " C"))
                  ),
                  
                  #theme et sortie du tableau et de la fenetre de recherche
                  theme = reactableTheme( 
                    searchInputStyle = list(width = "100%"),
                    
                    headerStyle = list(
                      "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),borderColor = "#555"),
                  ),
                  
                  #columns = list()
                  
                  ) #table1 contenant les donnees de df1
      })
      
      #-------------------------------------------------- Inserserer une daterange ------------------------------------------------
      output$daterange <- renderUI({
        dateRangeInput(inputId = "daterange",label = "Select a date range",
                       start = min(df1$date),   # Start date
                       end = max(df1$date),     # End date
                       min = min(df1$date),     # The min allowed date
                       max = max(df1$date),     # The max allowed date
                       format = "yyyy/mm/dd",   # The format of the date to display in the browser. try "mm/dd/yy"  
                       separator = "to"         # String to display between the start and end input boxes. try "to"
        )
        
      })
      
      #---------------------------------------------------------------------------------------------------------------------------
    }
    else{
      return(df1)
    }
    
    
    #--------------------------- Plot OdonT1 --------------------------
    output$histplot1<-renderPlot({
      plot(as.numeric(df1$TempOd1)~as.Date(df1$date), type="l", col="red", xlab="Date", ylab = "Temperatures",main="Temperatures du site 812 entre 2011 et 2017")
    })
    
    #Configuration du telechargement du graphe de l'Odon T1
    output$save_plot1<-downloadHandler(
      filename = function(){ paste("plot1","png",sep = ".")},
      
      content=function(file){
        png(file)
        plot(as.numeric(df1$TempOd1)~as.Date(df1$date), type="l", col="red", xlab="Date", ylab = "Temperatures",main="Temperatures du site 812 entre 2011 et 2017")
        dev.off()
      }
      
    ) #End save_plot1
    
    #--------------------------- Plot OdonT2 --------------------------
    output$histplot2<-renderPlot({
      plot(as.numeric(df1$TempOd2)~as.Date(df1$date), type="l", col="blue", xlab="Date", ylab = "Temperatures",main="Temperatures du site 813 entre 2011 et 2017")
    })
    
    #Configuration du telechargement du graphe de l'Odon T2
    output$save_plot2<-downloadHandler(
      filename = function(){paste("plot2","png",sep = ".")},
      
      content=function(file){
        png(file)
        plot(as.numeric(df1$TempOd2)~as.Date(df1$date), type="l", col="blue", xlab="Date", ylab = "Temperatures",main="Temperatures du site 813 entre 2011 et 2017")
        dev.off()
      }
      
    )#End save_plot2
    
    #--------------------------- Plot OdonT4 --------------------------
    output$histplot4<-renderPlot({
      plot(as.numeric(df1$TempOd4)~as.Date(df1$date), type="l", col="green", xlab="Date", ylab = "Temperatures",main="Temperatures du site 815 entre 2011 et 2017")
    })
    
    #Configuration du telechargement du graphe de l'Odon T4
    output$save_plot4<-downloadHandler(
      filename = function(){paste("plot4","png",sep = ".")},
      
      content=function(file){
        png(file)
        plot(as.numeric(df1$TempOd4)~as.Date(df1$date), type="l", col="green", xlab="Date", ylab = "Temperatures",main="Temperatures du site 815 entre 2011 et 2017")
        dev.off()
      }
      
    )#End ave_plot4
    
    #--------------------------- Plot OdonT5 --------------------------
    output$histplot5<-renderPlot({
      plot(as.numeric(df1$TempOd5)~as.Date(df1$date), type="l", col="yellow", xlab="Date", ylab = "Temperatures",main="Temperatures du site 816 entre 2011 et 2017")
    })
    
    #Configuration du telechargement du graphe de l'Odon T2
    output$save_plot5<-downloadHandler(
      filename = function(){paste("plot5","png",sep = ".")},
      
      content=function(file){
        png(file)
        plot(as.numeric(df1$TempOd5)~as.Date(df1$date), type="l", col="yellow", xlab="Date", ylab = "Temperatures",main="Temperatures du site 816 entre 2011 et 2017")
        dev.off()
      }
      
    )
    
    #------------------------------ Insertion de la date et du graphe de series temporelle -------------------------------------------
    ##1: 1ere synchronisation Temp et graphiques
    output$leplot6 <- renderPlot({
      plot(as.numeric(df1[[input$Temp]])~as.Date(df1$date), type="l", col="red", xlab="Date", ylab = "Temperatures",main=paste("Temperatures de ", input$Temp,"entre 2011 et 2017"))
    })
    
    #Telecharger leplot6
    output$save_leplot6<-downloadHandler(
      filename = function(){paste("Int_plot6","png",sep = ".")},
      
      content=function(file){
        png(file)
        plot(as.numeric(df1[[input$Temp]])~as.Date(df1$date), type="l", col="red", xlab="Date", ylab = "Temp",main=paste("Temperatures de ", input$Temp,"entre 2011 et 2017"))
        dev.off()
      }
      
    )
    
    #----------------------------
    output$leplot7 <- renderPlot({
      hist(as.numeric(df1[[input$Temp]]), type="l", col="grey", xlab = "Variations des Temp",main = paste("Histogramme des Temp de", input$Temp))
    })
    
    #Telecharger leplot7
    output$save_leplot7<-downloadHandler(
      filename = function(){paste("Int_plot7","png",sep = ".")},
      
      content=function(file){
        png(file)
        hist(as.numeric(df1[[input$Temp]]), type="l", col="grey", xlab = "Variations des Temp",main = paste("Histogramme des Temp de", input$Temp))
        dev.off()
      }
      
    )
    
    #-------------------------------------------------------------------------------------------------------------------------------------------------------
    # ###2: 2eme synchronisation 
    # library(lubridate)
    # # Input_date <- reactive({
    # #   filter(df1, between(as.numeric(TempOd1) ,input$daterange[1], input$daterange[2]))
    # # })
    # 
    # 
    # 
    # #filter(tweets, between(date ,input$date[1], input$date[2]))
    # # observe({updateDateRangeInput(session,inputId = "daterange")})     #Mise a jours de la date
    # 
    # #leplot6 et leplot7
    # #Affichage graphques
    # # plot(as.numeric(df1$TempOd1)~Input_date(), type="l", col="yellow", xlab="Date", ylab = "Temperatures",main="Temperatures entre 2011 et 2017")
    # output$leplot6 <- renderPlot({
    #   plot(dataInput(), type="l", col="yellow", xlab="Date", ylab = "Temperatures",main="Temperatures entre 2011 et 2017")
    # })
    
    
    #---------------------------------------------------------------------------------------------------------------
  })
  
  
  #-------------------------------------------------------- df2 -------------------------------------------------------------
  #Page2: Analyse des donnees Map
  output$Mapcontents <- renderPrint({
    req(input$file2)
    tryCatch(
      {#----Upload CSV Data File ----
        df2 <- read.csv(input$file2$datapat,header = input$header,row.names = 1,encoding = 'UTF-8',sep = input$sep, quote = input$quote)
      },
      
      #Renvoyer un safeError en cas d'erreur
      error = function(e) {
        stop(safeError(e)) 
      }
    )
    
    #----Conditions de sorties des donnees------
    if(input$disp == "head") {
      output$table2 <- renderReactable({ 
        reactable(df2)
        })
      
      #--------------------------------------
      #-- Interaction avec les donnees Map --
      filteredData <- reactive({
        if (input$sonde == "All probes") {
          df2
        } 
        else {
          filter(df2, lib_sonde == input$sonde)
        }
      })
      
      #Affichage MAP Carte
      output$map <- renderLeaflet({
        leaflet(filteredData()) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addMarkers(~as.numeric(lon_wgs84), ~as.numeric(lat_wgs84), 
                     labelOptions = labelOptions(textsize = "12px"),popup =~ lib_sonde)
      })
      
      #Interpretation d'infos avec Map lors de l'affichage
      observe({
        leafletProxy("map", data = filteredData()) %>%
          clearShapes() %>%
          addMarkers(~as.numeric(lon_wgs84), ~as.numeric(lat_wgs84), 
                     labelOptions = labelOptions(textsize = "12px"),popup = ~lib_sonde)
      })
      
      #--------------------------------------------------------------------------------------------------------------------------
    }
    else{
      return(df2)
    }
    
  }) #End Mapcontents
  
  #--------Accolade de fermeture --------
} #End function(input, output, session)

#************ Creation of Shiny App ************
# Create Shiny app ----
shinyApp(ui, server)
