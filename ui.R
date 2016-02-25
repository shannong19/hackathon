library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

dashboardPage(skin="yellow",
  dashboardHeader(title = "SPEW VIEW"),

 ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("Map", tabName="map", icon=icon("map")), 
      menuItem("Explore Time Series", tabName = "timeseries", icon = icon("th"))
    )
  ),

  ## Body content
  dashboardBody(
      tags$head(
          tags$link(rel = "stylesheet", type="text/css", href = "custom.css")
          ),
      tabItems(... = 
      # First tab content
      tabItem(tabName = "dashboard",
        fluidRow(
          box(plotOutput("plot1", height = 250)),

          box(
            title = "Controls",
            sliderInput("slider", "Number of observations:", 1, 100, 50)
          )
        ), 
        h2("Widgets tab content")
      ),

      # Second tab content
      tabItem(tabName = "widgets",
        h2("Widgets tab content")
      ),

      # Third tab content 
      tabItem(tabName = "map",
              fluidRow(
                  column(width = 9,
                         box(width = NULL, solidHeader = TRUE,
                             leafletOutput("map", height = 500)
                             ),
                         box(width = NULL,
                             uiOutput("numVehiclesTable")
                             )
                         ),
                  column(width = 3,
                         box(width = NULL, status = "warning",
                             uiOutput("routeSelect"),
                             checkboxGroupInput("directions", "Show",
                                                choices = c(
                                                    Northbound = 4,
                                                    Southbound = 1,
                                                    Eastbound = 2,
                                                    Westbound = 3
                                                ),
                                                selected = c(1, 2, 3, 4)
                                                ),
                             p(
                                 class = "text-muted",
                                 paste("Note: a route number can have several different trips, each",
                                       "with a different path. Only the most commonly-used path will",
                                       "be displayed on the map."
                                       )
                             ),
                             actionButton("zoomButton", "Zoom to fit buses")
                             ),
                         box(width = NULL, status = "warning",
                             selectInput("interval", "Refresh interval",
                                         choices = c(
                                             "30 seconds" = 30,
                                             "1 minute" = 60,
                                             "2 minutes" = 120,
                                             "5 minutes" = 300,
                                             "10 minutes" = 600
                                         ),
                                         selected = "60"
                                         ),
                             uiOutput("timeSinceLastUpdate"),
                             actionButton("refresh", "Refresh now"),
                             p(class = "text-muted",
                               br(),
                               "Source data updates every 30 seconds."
                               )
                             
                             )
                         )
                 
              )
            ), 
            
            # Time series explore content 
            tabItem(tabName = "timeseries",
                    h1("Time Series Tab")
                    
                    )
    )
  )
)
