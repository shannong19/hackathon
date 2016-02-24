#server


function(input, output) {

set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })


    #map
    output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })



    
## # 1=South, 2=East, 3=West, 4=North
## dirColors <-c("1"="#595490", "2"="#527525", "3"="#A93F35", "4"="#BA48AA")


        
##   # Store last zoom button value so we can detect when it's clicked
##   lastZoomButtonValue <- NULL

##   output$busmap <- renderLeaflet({
##     locations <- NULL
##     if (length(locations) == 0)
##       return(NULL)

##     # Show only selected directions
##     locations <- filter(locations, Direction %in% as.numeric(input$directions))

##     # Four possible directions for bus routes
##     dirPal <- colorFactor(dirColors, names(dirColors))

##     map <- leaflet(locations) %>%
##       addTiles('http://{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png') %>%
##       addCircleMarkers(
##         ~VehicleLongitude,
##         ~VehicleLatitude,
##         color = ~dirPal(Direction),
##         opacity = 0.8,
##         radius = 8
##       )

##     if (as.numeric(input$routeNum) != 0) {
##       route_shape <- get_route_shape(input$routeNum)

##       map <- addPolylines(map,
##         route_shape$shape_pt_lon,
##         route_shape$shape_pt_lat,
##         fill = FALSE
##       )
##     }

##     rezoom <- "first"
##     # If zoom button was clicked this time, and store the value, and rezoom
##     if (!identical(lastZoomButtonValue, input$zoomButton)) {
##       lastZoomButtonValue <<- input$zoomButton
##       rezoom <- "always"
##     }

##     map <- map %>% mapOptions(zoomToLimits = rezoom)

##     map
##   })

    
    
}
