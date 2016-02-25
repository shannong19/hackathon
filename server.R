source("global.R")

server <- function(input, output) {
  
  # Histogram Output 
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  
  # Mapping output 
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })  
  
  output$avail_locs <- renderUI({
    disease_index <- which(names(x = diseases) == input$disease)
    current_disease <- diseases[[disease_index]]    
    avail_locs <- as.list(colnames(current_disease))
    selectInput("avail_locs", "Location 2", avail_locs)
  })  
    
  # Display the results of the selection 
  output$text1 <- renderText({
    paste0("You have selected Location: ", input$location, " Disease: ", input$disease, 
           " and Time Period ", input$time)
  })
  
  output$disease_ts <- renderPlot({
    disease_index <- which(names(x = diseases) == input$disease)
    current_disease <- diseases[[disease_index]]    
    plot(as.numeric(current_disease$YEAR), as.numeric(current_disease[, 3]))
  })
  
  
  
}
