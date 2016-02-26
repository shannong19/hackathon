source("global.R")

# Title and Dashboard -------------------------------------

# Header for the SHiny App 
dashboardPage(skin="yellow",
  dashboardHeader(title = "SPEW VIEW"),

  # Set up the side-bar for the Dashboard 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Data Snapshot", tabName="snapshot", icon=icon("bar-chart")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("calendar")),
      menuItem("Correlations", tabName = "ds", icon = icon("line-chart")),
      menuItem("Map", tabName="map", icon=icon("map"), selected=TRUE),
      menuItem("Animations", tabName = "chloropleth", icon = icon("caret-square-o-right"))
    )
  ),
  
  # Body of User Interface -----------------------------------
  dashboardBody(
      
    # Set up the style  sheet 
    tags$head(
        tags$link(rel = "stylesheet", type="text/css", href = "custom.css")
        ),
    
    tabItems(
    

      
    # Mapping tab  
    tabItem(tabName = "map",
        fluidRow(
              column(width = 3,
                 box(width = NULL, status = "warning", solidHeader = TRUE,
                      selectInput("diseasemap", label = h3("Disease"), 
                              choices = disease_names, selected="DIPHTHERIA")
                     ),
                     box(width = NULL, status = "warning",
                      h3("Interactive Map"),
                      p("In this map, the radius of the circle is proportional to the disease incidence",  strong("Click on a circle"), "for the exact value.  You can", strong("scroll"), "over to Hawaii and Alaska as well!",  strong("Click the play button"), "at the bottom to see the diseases over time.")
                     )
                  ),
             column(width = 9,
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("map", height = 450)),
               box(width = NULL,
                   sliderInput("maptime", label = h5("Time (%)"), min = 0, 
        max = 100, value = 0, animate = TRUE)
        )
        )
        )
      ),


      tabItem(tabName = "ds",
              fluidRow(
                    
                  box(status = "primary", width=3, title= "Toggles",
                      uiOutput("x_value"),
                      radioButtons("radiods", label=h3("Choose primary view"),
                                   choices = list("States' Incidence" = 1, "Distance and Incidence" = 2)),
                      selectInput("diseaseds", label = h3("Disease"), 
                                  choices = disease_names, selected="POLIO"),
                      dateRangeInput("timeds", label = h3("Time Range"),
                                     min="1860-01-01",
                                     max = "2015-12-31",
                                     start ="1860-01-01",
                                     end = "2015-12-31"),

                      uiOutput("cor_locs")
                      ),
    
      
                  box(title = "Correlation Plots",  width=9, status="warning", solidHeader=TRUE,
                      plotOutput("cor", height=800, hover="hover_ds")
                      )
              )

    ),
    
  # Time series Tab -----------------    
  tabItem(tabName = "timeseries",
    h1("Time Series Tab"), 
    fluidRow(
      box(
        selectInput("disease", label = h3("Disease"), 
                    choices = disease_names, selected="SMALLPOX"),
        uiOutput("avail_locs"), 
        uiOutput("avail_years"),  
        width = 3
      ), 
      box(
        textOutput("text1"), 
        plotOutput("disease_ts", height = 650), 
        width = 8 
        )
      )
    ), 
  
  tabItem(tabName = "chloropleth", 
          h1("Movie Tab"), 

          fluidRow(
            box(selectInput("disease_chlor", label = h3("Disease"), 
                  choices = disease_names_nodip), 
                uiOutput("avail_years_chlor"), 
                h3("Download Animation Below"),
                downloadButton("testgif"), 
                width = 3
              ), 
            box(
              plotOutput("chloropleth", height = 650), 
              width = 8 
              )
            )
          ),


   tabItem(tabName="home",
        box(status = "warning", solidHeader = TRUE,
           title=h1("Welcome to SPEW VIEW."),
           p("SPEW VIEW is a tool for visualizing historical diseases in the United States including"),
           p("1. Diphtheria"),
           p("2. Hepatitis A"),
           p("3. Measles"),
           p("4. Mumps"),
           p("5. Pertusis"),
           p("6. Polio"),
           p("7. Rubella"),
           p("8. Smallpox."),
           p("All data is from", a("Project Tycho", href="https://www.tycho.pitt.edu/", target="_blank"), ".")
           ),
        box(status="warning", solidHeader=TRUE, title=h1("Features"),
            h3("Time Series - Look at diseases over time"),
            h3("Summary Data - Basic features of the data"),
            h3("Interactive Map - Explore the US"),
            h3("Animations - Smoothed data over time"),
            h3("Table Viewer - Snapshot of the data")
            )
        )        
        ),

  tabItem(tabName = "snapshot", title= h3("Snapshot"),
          fluidPage(

                                        # Create a new Row in the UI for selectInputs
              fluidRow( box(width=12, title="Snapshot", solidHeader=TRUE, status="warning",
                            p("Look at the data in row format.  Pick the disease, location, and date range."),
                  column(3,
                         selectInput("disease_snap",
                                     "Disease", choices = disease_names, selected="POLIO")
                         ),
                  
                  column(3,
                          uiOutput("snap_locs")
                         ),
                  column(3,
                          uiOutput("snap_years")
                         ),
                  column(3,
                         downloadButton("downloadData", "Download (.csv)")
                         )
              )           
              ),
 
                                        # Create a new row for the table.
              fluidRow(
                  DT::dataTableOutput("table")
              )
          )

          )
    )
  )
