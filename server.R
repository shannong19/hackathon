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

    #  dt <- as.Date("1966-01-02")
      disease_name <- input$diseasemap
      fulldf <- diseases[[disease_name]]
      print(dim(fulldf))
      fulldf <- fulldf[order(fulldf$date),]
      n <- nrow(fulldf)
      # get date from slider
      print(input$maptime)
      if (input$maptime == 0){
          dt <- fulldf$date[1]  
      } else if (input$maptime == 100){
          dt <- fulldf$date[nrow(fulldf)] 
      } else {   
          dt <- fulldf$date[ ceiling(input$maptime / 100 * n) ]
      }
      print(dt)
      df <- subset(fulldf, date==dt)
      
      mega_df <-  do.call('rbind', lapply(3:(ncol(df)-1), function(i) {
          subdf <- df[,c( ncol(df), i)] #date then incidence
          subdf$NAME_1<- colnames(subdf)[2] #make a state id
          colnames(subdf)[1:2] <- c("date", "incidence")
          return(subdf)
          }))
      mega_df$incidence <- as.numeric(mega_df$incidence)

      #join with us map data
     # us_data <- join(usa_shape@data[, c(1,5)], mega_df, by="NAME_1")
      #usa_shape@data <- us_data
      center_df$NAME_1 <- center_df$region
      usa_data <- join(center_df, mega_df, "NAME_1")
      usa_data$popup <- paste(usa_data$NAME_1, "\n", usa_data$incidence, "incidence")
      pal <- colorQuantile("Purples", NULL, n=5)

      leaflet(data=usa_data) %>%
          addTiles() %>%
          addCircles(lng = ~Longitude, lat = ~Latitude, weight=1, radius = ~(1*10^5*sqrt(incidence)), popup= ~popup, color="red") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
          addLegend(position = "bottomleft", title=dt, color="red", opacity=.8, labels="Incidence")
          
      
    ## leaflet() %>%
    ##   addTiles(
    ##     urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    ##     attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ##   ) %>%
    ##   setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

    # Print the name of the x value
    output$x_value <- renderText({
        if (is.null(input$hover_ds$x)) return("Hover over plot")
        if ( input$radiods == 1){
            out <- "This correlation plot tells us if the states are correlated with one another.  The redder the square is, then an increase of the disease in the square's row state is associated with an increase of the disease in the square's column state."
        } else {
            out <- "This plot shows us the effect of distance on correlation.  A downward trend line implies that as states become farther apart, the less likely their incidences are to have the same pattern.  A smoothed trend line is fit to the data with a 99% Confidence Interval. "
        }
        out
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
    disease_index<- which(names(x = diseases) == input$disease)  
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

  ## output$disease_ts <- renderPlot({
  ##   # Subset the disease and obtain the location and 
  ##   # time indices 
  ##   disease_index <- which(names(x = diseases) == input$disease)
  ##   current_disease <- diseases[[disease_index]]
  ##   location_index <- which(colnames(current_disease) == input$avail_locs)
  
  ##   plot(current_disease$date, as.numeric(current_disease[, location_index]), 
  ##       main = title, xlab = "Time", ylab = "Count per 100,000", 
  ##       xlim = c(start_time, end_time))
  ## })
  
  output$avail_years_chlor <- renderUI({
    disease_index <- which(names(x = diseases) == input$disease_chlor)  
    current_disease <- diseases[[disease_index]]
    dates <- current_disease$date     
    dateRangeInput("avail_years_chlor", 
                   label = h3("Date Range"), 
                   start = dates[1], 
                   end = dates[nrow(current_disease)])    
  })
  
  output$chloropleth <- renderPlot({    
    # Subset the disease list and the appropriate dates 
    disease_index <- which(names(x = diseases) == input$disease_chlor)
    current_disease <- diseases[[disease_index]]
    
    dates <- current_disease$date
    start_time <- input$avail_years_chlor[1] - 5
    end_time <- input$avail_years_chlor[2] + 5
    date_range <- which(dates > start_time & dates < end_time)
    print(dates)
    print(start_time)
    print(end_time)
    current_disease <- current_disease[date_range, ]

    non_data_cols <- which(names(current_disease) %in% c("YEAR", "WEEK", "date"))
    tmp_data <- current_disease[, -non_data_cols]
    tmp_data <- sapply(tmp_data, as.numeric)
    
    # Get the column wise averages for each location 
    # and turn this into a vector 
    state_means <- as.numeric(colMeans(tmp_data, na.rm = TRUE))
    state_names <- gsub(pattern = "\\.", " ", colnames(tmp_data))
    id <- tolower(state_names)
    value_df<- data.frame(id, state_means)
    value_df$id <- as.character(value_df$id)    
    plot_data <- left_join(us_fortify, value_df)
    
    # Plot the resulting image along with us_fortify
    ggplot() + geom_polygon(data = plot_data, 
                  aes(x = long, y = lat, group = group, fill = state_means), 
                  color = "black", size = 0.25)
  })

  output$disease_ts <- renderPlot({
    # Subset the disease and obtain the location and 
    # time indices 
    disease_index <- which(names(x = diseases) == input$disease)
    current_disease <- diseases[[disease_index]]
    location_index <- which(colnames(current_disease) == input$avail_locs)

             start_time <- input$avail_years[1] - 5
          end_time <- input$avail_years[2] + 5
          title <- paste0(input$disease, " In ", input$avail_locs)
          
           plot(current_disease$date, as.numeric(current_disease[, location_index]), 
               xlab = "Time", ylab = "Count per 100,000", 
               xlim = c(start_time, end_time), col="gold")

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
