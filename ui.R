source("global.R")

# Title and Dashboard -------------------------------------

# Header for the SHiny App 
dashboardPage(skin="yellow",
  dashboardHeader(title = "SPEW VIEW"),

  # Set up the side-bar for the Dashboard 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Animated Maps", tabName="map", icon=icon("map")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("calendar")),
      menuItem("Correlations", tabName = "ds", icon = icon("line-chart")),
      menuItem("Clustering", tabName="clust", icon=icon("object-group")),
      menuItem("Choropleth Maps", tabName = "chloropleth", icon = icon("caret-square-o-right")),
       menuItem("Data Snapshot", tabName="snapshot", icon=icon("bar-chart"))
    )
  ),
  
  # Body of User Interface -----------------------------------
  dashboardBody(
      
    # Set up the style  sheet 
    tags$head(
        tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }", 
                             ".shiny-output-error:before { visibility: hidden; }" ),
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
                      plotOutput("cor", height=500, hover="hover_ds")
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
                                                          width = 3, 
                                                          h4("Time Series Explanation: "), 
                                                          p("This view allows users to compare the time series of the eight diseases across time and location. Specifically, choose one of the eight diseases, then select any number of locations to view their time series. Finally, you can specify a time period in which to compare the incidence counts as well. We hope that this provides an easy way to view and compare disease trends over time")
                                                        ), 
                                                        box(
                                                          plotOutput("disease_ts", height = 650), 
                                                          width = 8 
                                                        )
                                                      )
                                              ), 

  #clustering
  tabItem(tabName = "clust",
          box(title="State Profiles", width=3, status="warning", solidHeader=TRUE,
              selectInput("clust_method", label= h4("Clustering Method"), choices = c("Hierarchical", "k-means", "Model-Based"), selected="k-means"),
              selectInput("decade", label = h4("Decade"), choices = seq(1920, 2010, by=10)),
              sliderInput("nclust", label = h4("Number of Clusters (k-means, MB)"), min=2, max=10, value=4, step=1)
              ),
          
                 
                 
          box(title="Clustering the States by Decade-Long Disease Incidence Profiles", width=9,
              plotOutput("clust", height=500)
              )
                 
          ),
  
  tabItem(tabName = "chloropleth", 
                                                      h1("Movie Tab"), 
                                                      
                                                      fluidRow(
                                                        box(selectInput("disease_chlor", label = h3("Disease"), 
                                                                        choices = disease_names_nodip), 
                                                            uiOutput("avail_years_chlor"), 
                                                            h3("Download animation: "),
                                                            downloadButton("testgif"), 
                                                            width = 3, 
                                                            h4("Choropleth explanation: "), 
                                                            p("This tab allows you to create custom choropleth maps for any disease during any time period. You can also download the .gif animation file, which will display an evolution of a disease, by time over all of the years with data")
                                                        ), 
                                                        box(
                                                          plotOutput("chloropleth", height = 650), 
                                                          width = 8 
                                                        )
                                                      )
                                              ),


   tabItem(tabName="home",
        fluidRow(
            box(status = "warning", solidHeader = TRUE, width=12,
           title=h2("Welcome to SPEW VIEW."),
           p(strong("SPEW VIEW"), "is a tool for visualizing historical diseases in the United States.  All data is from", a("Project Tycho", href="https://www.tycho.pitt.edu/", target="_blank"), "."),
           p("Navigate the tabs to the left to find downloadable data, animated maps, choropleth maps, clustering analysis, time series analysis, and interactive data visualizations."),
           h3("Meet the Team"),
           h3(a("Department of Statistics", href="http://www.stat.cmu.edu"), "Carnegie Mellon University"),
               img(src="Shannon_Gallagher.JPG", align="left", width=200),
               img(src="Lee_Richardson.JPG", align="left", width=200),
               img(src="sam.jpg", align="left", width=200),
               img(src="Bill_Eddy_Headshot.jpg", align="left", width=200)
           )
           ),
           
        fluidRow(
#        box(width=12, status="warning",
            box(width=12,
           h4(a("Shannon Gallagher", href="http://www.stat.cmu.edu/~sgallagh"), "is a second year PhD Student in Statistics at CMU"),
           h4(a("Lee Richardson", href="http://www.stat.cmu.edu/~lrichard"), "is a second year PhD Student in Statistics at CMU"),
           h4(a("Sam Ventura", href="http://www.stat.cmu.edu/~sventura"), "is a Visting Assistant Professor of Statistics at CMU"),
           h4(a("Bill Eddy", href="http://www.stat.cmu.edu/~bill"), "is the John C. Warner Professor of Statistics at CMU")
           )
           )
        
      #     )
  ),
           
        
  
  tabItem(tabName = "snapshot", title= h3("Snapshot"),
          fluidPage(

                                        # Create a new Row in the UI for selectInputs
              fluidRow( box(width=12, title="Snapshot", solidHeader=TRUE, status="warning",
                            p("Look at the data in row format.  Pick the disease, location(s), and date range."),
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
)
