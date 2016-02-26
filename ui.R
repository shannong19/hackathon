source("global.R")

# Title and Dashboard -------------------------------------

# Header for the SHiny App 
dashboardPage(skin="yellow",
  dashboardHeader(title = "SPEW VIEW"),

  # Set up the side-bar for the Dashboard 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Explore Time Series", tabName = "timeseries", icon = icon("th")),
      menuItem("Summary Data", tabName = "ds", icon = icon("th")),
      menuItem("Map", tabName="map", icon=icon("map")),
      menuItem("Animations", tabName = "chloropleth", icon = icon("th"))
    )
  ),
  
  # Body of User Interface -----------------------------------
  dashboardBody(
      
    # Set up the stylesheet 
    tags$head(
        tags$link(rel = "stylesheet", type="text/css", href = "custom.css")
        ),
    
    tabItems(
    
    # Dashboard Tab 
    tabItem(tabName = "Home",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),
      
    # Mapping tab  
    tabItem(tabName = "map",
        fluidRow(
            column(width = 9,
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("map", height = 450)),
               box(width = NULL,
                   sliderInput("maptime", label = h5("Time (%)"), min = 0, 
        max = 100, value = 0)
        )
               ),
              column(width = 3,
                 box(width = NULL, status = "success", solidHeader = TRUE,
                      selectInput("diseasemap", label = h3("Disease"), 
                              choices = disease_names, selected="MUMPS")
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
                         "Source data updates every 30 seconds.")    
                     )
                  )           
        )
      ),


      tabItem(tabName = "ds",
              fluidRow(
    box(title = "Correlation Plots",  width=9, status="warning", solidHeader=TRUE,
        plotOutput("cor", height=800)
        ),
    box(status = "primary", width=3,
          radioButtons("radiods", label=h3("Choose primary view"),
                    choices = list("States' Incidence" = 1, "Distance and Incidence" = 2)),
         selectInput("diseaseds", label = h3("Disease"), 
                              choices = disease_names, selected="POLIO"),
        uiOutput('avail_locs2'), 
        dateRangeInput("timeds", label = h3("Time Range"),
                       min="1860-01-01",
                       max = "2015-12-31",
                       start ="1860-01-01",
                       end = "2015-12-31")
        )
  ),

  fluidRow(
    box(
       width = 4, solidHeader = TRUE, status = "primary", title="Data View",
         
                                        # Copy the line below to make a set of radio buttons
       radioButtons("radio", label="Choose primary view",
                    choices = list("Location" = 1, "Disease" = 2), 
                    selected = 1)
    ),
    box(
      title = "Title 2", width = 4, solidHeader = TRUE,
      "Box content"
    ),
    box(
      title = "Title 1", width = 4, solidHeader = TRUE, status = "warning",
      "Box content"
    )
  ),

  fluidRow(
    box(
      width = 4, background = "black",
      "A box with a solid black background"
    ),
    box(
      title = "Title 5", width = 4, background = "light-blue",
      "A box with a solid light-blue background"
    ),
    box(
      title = "Title 6",width = 4, background = "maroon",
      "A box with a solid maroon background"
    )
  )
  ),
    
  # Time series Tab -----------------    
  tabItem(tabName = "timeseries",
    h1("Time Series Tab"), 
    fluidRow(
      box(
        selectInput("disease", label = h3("Disease"), 
                    choices = disease_names),
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
          h1("Movie Tab")
          )
    
    )
  )
)
