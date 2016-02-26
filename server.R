source("global.R")

# Update the US Shapefile to work with ggplot2 
us_fortify <- fortify(usa_shape, region = "NAME_1")
alaska_hawaii_df <- which(center_df$region %in% c("Alaska", "Hawaii"))
alaska_hawaii_shape <- which(us_fortify$id %in% c("Alaska", "Hawaii"))
us_fortify <- us_fortify[-alaska_hawaii_shape, ]    
center_df <- center_df[-alaska_hawaii_df, ]
center_df$val <- rnorm(n = nrow(center_df), mean = 10, sd = 20)

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
  
  # Adaptive user choices ---------------------------------
  output$avail_locs <- renderUI({
    disease_index <- which(names(x = diseases) == input$disease)
    current_disease <- diseases[[disease_index]]    
    location_names <- colnames(current_disease)[3:(ncol(current_disease) - 1)]
    avail_locs <- as.list(location_names)
    selectInput("avail_locs", h3("Location"), avail_locs)
  })  
    
  output$avail_years <- renderUI({
    disease_index <- which(names(x = diseases) == input$disease)  
    current_disease <- diseases[[disease_index]]    
    dates <- current_disease$date     
    dateRangeInput("avail_years", 
                   label = h3("Date Range"), 
                   start = dates[1], 
                   end = dates[nrow(current_disease)])    
  })
  
  # Display the results of the selection 
  output$text1 <- renderText({
    paste0("You have selected Location: ", input$avail_locs, " Disease: ", input$disease)
  })

  output$disease_ts <- renderPlot({
    # Subset the disease and obtain the location and 
    # time indices 
    disease_index <- which(names(x = diseases) == input$disease)
    current_disease <- diseases[[disease_index]]
    location_index <- which(colnames(current_disease) == input$avail_locs)
    
    # Plot the time series plot 
    start_time <- input$avail_years[1] - 5
    end_time <- input$avail_years[2] + 5
    title <- paste0(input$disease, " In ", input$avail_locs)
    plot(current_disease$date, as.numeric(current_disease[, location_index]), 
         main = title, xlab = "Time", ylab = "Count per 100,000", 
         xlim = c(start_time, end_time))
  })

  output$chloropleth <- renderPLot({    
    ggplot() + geom_map(data = center_df, aes(map_id = region, fill = val), 
                        map = us_fortify) + expand_limits(x = us_fortify$long, y = us_fortify$lat)
  })  
  
    output$timeds <- renderUI({
    disease_index <- which(names(x = diseases) == input$disease)  
    current_disease <- diseases[[disease_index]]    
    dates <- current_disease$date     
    dateRangeInput("timeds", 
                   label = h3("Date Range"), 
                   start = min(dates), 
                   end = max(dates))    
  })

    #correlation plot
    output$cor<- renderPlot({
       # disease_name <- "SMALLPOX"
        disease_name <- input$diseaseds
        df <- diseases[[disease_name]]
        #subset df to proper time range
        print(input$timeds)
        df <- subset(df, df$date >= input$timeds[1] & df$date <= input$timeds[2])      
        print(head(df))
        print(tail(df))
        
           
        nms <- colnames(df)
        mat <- data.matrix(df[, -c(1,2, ncol(df))])
        cormat <- cor(mat, use="pairwise.complete.obs", method="spearman")
        melted_cormat <- melt(cormat)

           #distance and incidence
        if (input$radiods == 2){
          if (input$diseaseds == "DIPHTHERIA"){
            plot(1,1, main="Under Construction")
        } else {
            cors <- as.vector((cormat))
            dist <- as.vector(t(state_dist))
           # plot(dist, cors)
           my_df <- data.frame(dist=dist, cors=cors)
            p <- ggplot(my_df, aes(dist, cors)) + geom_point(colour="gold", size=2) +
                geom_smooth(level=.999, colour="blue", fill="blue")+ ggtitle(disease_name) +
                 labs(x="Distance (Scaled)", y="Correlation") +
                theme_minimal()  
            print(p)
        }
            

      } else{
        
            g <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile(color="white") +
                scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                 midpoint = 0, limit = c(-1,1), space = "Lab", 
                                 name="Spearman\nCorrelation") +
                theme_minimal()+ 
                theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                                 size = 8, hjust = 1),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank())+
                coord_fixed() + ggtitle(disease_name)
            print(g)
        }
    })
}
