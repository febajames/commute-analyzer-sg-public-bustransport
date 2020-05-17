### COMMUTE ANALYZER ####

# Load packages via explicit library calls for shinyapps.io
# install.packages('shiny')
# install.packages('shinydashboard')
# install.packages('tidyverse')
# install.packages('reshape2')
# install.packages('sf')
# install.packages('tmap')
# install.packages('leaflet')
# install.packages('RColorBrewer')
# install.packages('igraph')
# install.packages('ggraph')
# install.packages('heatmaply')
# install.packages('plotly')
# install.packages('shinyjs')
# install.packages('shinycssloaders')
# install.packages("dplyr")

library(shinydashboard)
library(magrittr)
library(dplyr)
library(leaflet)
library(shinycssloaders)
library(shiny)
library(heatmaply)
library(plotly)
library(ggraph)
library(igraph)
library(RColorBrewer)
library(tmap)
library(sf)
library(reshape2)
library(tidyverse)
library(ggplot2)

options(spinner.color.background="#F5F5F5")

# ui.R
sidebar <- dashboardSidebar(
  sidebarMenu(
    selectInput(
      inputId = "boundary",
      label = "Planning Boundary",
      choices = c("PLANNING AREA", "SUBZONE"),
      selected = "PLANNING AREA"
    ),
    
    selectInput(
      inputId = "day",
      label = "Day Type",
      choices = c("WEEKDAY", "WEEKENDS/HOLIDAY"),
      selected = "WEEKDAY"
    ),
    
    sliderInput(
      inputId = "hour",
      label = "Hour of Day (24h)",
      min = 0,
      max = 23,
      value = 7,
      step = 1
    )
  )
)

body <- dashboardBody(
  tabBox(width = 100,
         # The id lets us use input$tabset1 on the server to find the current tab
         id = "tabset1",
    tabPanel("Outflows & Inflows", value = 1,
             fluidRow(
               column(width = 6,
                      box(title = "Outflow from Planning Boundary",
                          width = 12,
                          status="primary",
                          solidHeader = TRUE,
                          leafletOutput("from_c", height = 400) %>% withSpinner(type = 3, color.background = getOption("spinner.color.background"), size= 1,
                                                                                color="skyblue")
                       )
                      ),
               column(width = 6,
                      box(title = "Inflow to Planning Boundary",
                          width = 12,
                          status="primary",
                          solidHeader = TRUE,
                          leafletOutput("to_c", height = 400) %>% withSpinner(type = 3, color.background = getOption("spinner.color.background"), size= 1,
                                                                              color="skyblue")
                      )
                      )
                    )
             ),
                   
    tabPanel("Interaction Strength", value=2,
             fluidRow(
               box(title = "Selection",
                   width = 12, 
                   status="primary",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   mainPanel(
                     radioButtons("intraconnect", "Intra-zonal Interaction",
                                  choices = c(Include = "in", Exclude = "out"),
                                  selected = "in"),
                     radioButtons("normalize", "Please select Yes/No to normalize trip counts",
                                  choices = c(Yes = "yes", No = "no"),
                                  selected = "no")
                   )
               ),
               box(title = "Adjacency Matrix of Inter and Intra Planning Area Interactions",
                   width = 12, 
                   status="primary",
                   solidHeader = TRUE,
                   mainPanel(
                     plotlyOutput(outputId = "plot1", width = "150%", height = "600px")
                   )
                   )
               
    )
    ),
    
    tabPanel("Functional Transport Areas", value = 3,
             tabBox(width = 100,
                    id ="tabset3",
                    tabPanel("Intramax Analysis - Clusters",
                             fluidRow(
                                      box(title = "Cumulative Intra-zonal Interaction Selection",
                                      width = 12,
                                      status="primary",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      column(width = 4, uiOutput("allocation"))
                                      )
                                    ),
                             fluidRow(
                                      box(title = "Functional Transport Area Clusters",
                                      width = 12,
                                      status="primary",
                                      solidHeader = TRUE,
                                      leafletOutput("map", height = 570) %>% withSpinner(type = 3, color.background = getOption("spinner.color.background"), size= 1,
                                                                                         color="skyblue")
                                      )
                                  )
                             ),
                               
                    tabPanel("Intramax Analysis - Dendrogram", 
                             fluidRow(
                                      box(title = "Dendrogram", 
                                          width = 12,
                                          status="primary",
                                          solidHeader = TRUE,
                                          plotOutput("dendro", height = 1000) %>% withSpinner(type = 3, color.background = getOption("spinner.color.background"), size= 1,
                                                                                              color="skyblue")
                                      )
                             )
                                      
                    )
             )
    ),
    
      tabPanel("User Guide", value = 4,
             uiOutput("pdf_viewer")
    
   ),
    
    tags$head(
      tags$style(HTML('
          .main-header .logo {
          font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 18px;
          }
                              ')),
      tags$style(type='text/css', 
                 ".nav-tabs {font-size: 16px}"),
      
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
                 ),
      # link to css file to reposition leaflet map controls
      tags$link(rel = "stylesheet", type = "text/css", href = "map.css")
                
    )
))

# Put them together into a dashboardPage
dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Commute Analyzer"),
  sidebar,
  body
)

