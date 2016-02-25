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
    paste0("You have selected Location: ", input$location, " Disease: ", input$disease, 
           " and Time Period ", input$time)
  })

  output$disease_ts <- renderPlot({
    # Subset the disease and obtain the location and 
    # time indices 
    disease_index <- which(names(x = diseases) == input$disease)
    current_disease <- diseases[[disease_index]]
    location_index <- which(colnames(current_disease) == input$avail_locs)
    
    # Plot the time series plot 
    print(str(input$avail_years))
    start_time <- input$avail_years[1] - 5
    end_time <- input$avail_years[2] + 5
    print(c(start_time, end_time))
    title <- paste0(input$disease, " In ", input$avail_locs)
    plot(current_disease$date, as.numeric(current_disease[, location_index]), 
         main = title, xlab = "Time", ylab = "Count per 100,000", 
         xlim = c(start_time, end_time))
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

#   output$avail_locs <- renderUI({
#       disease_index <- which(names(x = diseases) == input$diseaseds)
#       current_disease <- diseases[[disease_index]]    
#       avail_locs <- as.list(colnames(current_disease))
#       selectInput("avail_locs", "Location 2", avail_locs)
#       disease_name <- names(diseases)[disease_index]
#     })  



    

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
