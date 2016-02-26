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
      if (input$diseasemap != "DIPHTHERIA"){
          center_df$NAME_1 <- center_df$region
          usa_data <- join(center_df, mega_df, "NAME_1")
      } else {
          usa_data <- join(city_lookup, mega_df, "NAME_1")
      }
      
      # add in state pop
      yr <- as.numeric(format(dt, "%Y"))
      yr <- paste0("X", floor(yr/10) * 10) #get a census year
      print(yr)
      print('state pop')
      state_pop_df <- state_pops[,c("NAME_1", yr)]
      print(head(state_pop_df))
      colnames(state_pop_df)[2] <- "pop"
      usa_data <- join(usa_data, state_pop_df, "NAME_1")
      usa_data$popup <- paste0(usa_data$NAME_1, "; Incidence: ", usa_data$incidence, "; Pop: ", prettyNum(usa_data$pop, big.mark=","))
            
      #pal <- colorQuantile("Purples", NULL, n=5)

      leaflet(data=usa_data) %>%
          addTiles() %>%
          addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight=1, radius = ~(10*sqrt(incidence)), popup= ~popup, color="goldenrod", fillOpacity=.5) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
          addLegend(position = "bottomleft", title=paste(input$diseasemap, ";", dt), color="goldenrod", opacity=.8, labels="Incidence")          
  })

    # Print the name of the x value
    output$x_value <- renderText({
        if (is.null(input$hover_ds$x)) return("Hover over plot")
        if ( input$radiods == 1){
            out <- "This correlation plot tells us if the states are correlated with one another.  The redder the square is, then more of an increase of the disease in the square's row state is associated with an increase of the disease in the square's column state."
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
  
  # Time Series ------------------------------------
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
    
    start_time <- input$avail_years[1] - 5
    end_time <- input$avail_years[2] + 5
    title <- paste0(input$disease, " In ", input$avail_locs)
    
    plot(current_disease$date, as.numeric(current_disease[, location_index]), 
         xlab = "Time", ylab = "Count per 100,000", 
         xlim = c(start_time, end_time), col="gold")
  })
  
  # Chloropleth ----------------------------------------------
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
    current_disease <- current_disease[date_range, ]

    non_data_cols <- which(names(current_disease) %in% c("YEAR", "WEEK", "date"))
    tmp_data <- current_disease[, -non_data_cols]
    tmp_data <- sapply(tmp_data, as.numeric)
    
    # Get the column wise averages for each location 
    # and turn this into a vector 
    incidence <- as.numeric(colMeans(tmp_data, na.rm = TRUE))
    state_names <- gsub(pattern = "\\.", " ", colnames(tmp_data))
    id <- tolower(state_names)
    value_df<- data.frame(id, incidence)
    value_df$id <- as.character(value_df$id)    
    plot_data <- left_join(us_fortify, value_df)
    
    # Plot the resulting image along with us_fortify
    ggplot() + geom_polygon(data = plot_data, 
                  aes(x = long, y = lat, group = group, fill = incidence), 
                  color = "black", size = 0.25) + scale_fill_distiller(palette = "Spectral")
  })
  
  # Animation -------------------------------------------------  
  output$testgif = downloadHandler(
    filename = paste0(input$disease_chlor, '.gif'),   
    content  = function(file) {
      
      disease_index <- which(names(x = diseases) == input$disease_chlor)
      current_disease <- diseases[[disease_index]]
      
      mega_df <-  do.call('rbind', lapply(3:(ncol(current_disease)-1), function(i) {
        subdf <- current_disease[,c( ncol(current_disease), i)] # date then incidence
        subdf$NAME_1<- colnames(subdf)[2] # make a state id
        colnames(subdf)[1:2] <- c("date", "incidence")
        return(subdf)
      }))
      
      names(mega_df)[3] <- "region"  
      mega_df$region <- gsub(pattern = "\\.", " ", mega_df$region)
      
      # Remove alaska and hawaii, append on the latitude 
      # and longitude, and 
      alaska_hawaii <- which(mega_df$region %in% c("ALASKA", "HAWAII"))
      mega_df <- mega_df[-alaska_hawaii, ]
      
      mega_df <- left_join(mega_df, center_df)
      names(mega_df)[c(4, 5)] <- c("lon", "lat")
      mega_df$incidence <- as.numeric(mega_df$incidence)    
      mega_df$year <- as.numeric(format(mega_df$date,'%Y'))
      
      # Obrain a map of the USA 
      usa_map <- borders("usa", colour="gray50", fill="white")
      
      saveGIF({
        start_year <- min(mega_df$year)
        end_year <- max(mega_df$year)
        for (year in start_year:end_year) {
          year_df <- mega_df[which(mega_df$year == year), ]
          
          # Turn the data into a ggplot2 format 
          plot_data <- ddply(.data = year_df, .variables = c("region", "lon", "lat"), 
                             .fun = summarise,
                             incidence = mean(incidence, na.rm = TRUE)
          )
          plot_data <- plot_data[!is.na(plot_data$incidence), ]
          if (nrow(plot_data) < 5) {
            next
          }
          
          year_plot <- ggplot(data = plot_data, aes(x = lon, y = lat, col = incidence)) + 
            usa_map + geom_point(size = 10) + ggtitle(paste0("Year: ", year))
          print(year_plot)
        }
      }, movie.name = "random.gif", interval = 1)
      file.rename('random.gif', paste0(input$disease_chlor, '.gif'))  
    }
  )

  output$timeds <- renderUI({
    disease_index <- which(names(x = diseases) == input$disease)  
    current_disease <- diseases[[disease_index]]    
    dates <- current_disease$date     
    dateRangeInput("timeds", 
                   label = h3("Date Range"), 
                   start = min(dates), 
                   end = max(dates))    
  })

    #table viewer
    # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- diseases[[input$disease_snap]]

    disease_index <- which(names(x = diseases) == input$disease_snap)
    current_disease <- diseases[[disease_index]]
    location_index <- which(colnames(current_disease) %in% input$snap_locs)

    data <- subset(data, subset= (data$date >= input$snap_years[1] & data$date <= input$snap_years[2]) )
    data <- data[, c(1,2, location_index)]

    data
  }))


    # download table viewer

    output$downloadData <- downloadHandler(
        filename = function() { paste('spewview_tab', '.csv', sep='') },
        content = function(file) {
            data <- diseases[[input$disease_snap]]

            disease_index <- which(names(x = diseases) == input$disease_snap)
            current_disease <- diseases[[disease_index]]
            location_index <- which(colnames(current_disease) %in% input$snap_locs)

            data <- subset(data, subset= (data$date >= input$snap_years[1] & data$date <= input$snap_years[2]) )
            data <- data[, c(1,2, location_index)]

            write.csv(data, file)
    }
  )

    ## years for table viewer
    output$snap_years <- renderUI({
        disease_index <- which(names(x = diseases) == input$disease_snap)  
        current_disease <- diseases[[disease_index]]    
        dates <- current_disease$date     
        dateRangeInput("snap_years", 
                       label = "Date Range",
                       min = min(dates),
                       max = max(dates),
                       start = min(dates), 
                       end = max(dates))    
  })

    # locations for table viewer
  output$snap_locs<- renderUI({
    disease_index <- which(names(x = diseases) == input$disease_snap)
    current_disease <- diseases[[disease_index]]    
    location_names <- colnames(current_disease)[3:(ncol(current_disease) - 1)]
    snap_locs <- as.list(location_names)
    if( input$disease_snap != "DIPHTHERIA"){
        d_sel <- c("WASHINGTON", "CALIFORNIA", "COLORADO")
    } else {
        d_sel <- c("PITTSBURGH.PA", "PHILADELPHIA.PA", "SCRANTON.PA")
    }
    selectInput("snap_locs", "Location", snap_locs, multiple = TRUE, selected = d_sel)
  })  



    # correlation locations
    output$cor_locs<- renderUI({
        disease_index <- which(names(x = diseases) == input$diseaseds)
        current_disease <- diseases[[disease_index]]    
        location_names <- colnames(current_disease)[3:(ncol(current_disease) - 1)]
        print(location_names)
        cor_locs<- location_names
        if( input$diseaseds != "DIPHTHERIA"){
            d_sel <- c("PENNSYLVANIA", "OHIO", "NEW.YORK", "MARYLAND", "WEST.VIRGINIA", "VIRGINIA", "MICHIGAN", "ILLINOIS", "INDIANA", "DELAWARE")
        } else {
            d_sel <- c("CLEVELAND.OH", "COLUMBUS.OH", "TOLEDO.OH", "CINCINNATI", "PITTSBURGH.PA", "PHILADELPHIA.PA", "SCRANTON.PA", "READING.PA", "WILKES.BARRE.PA", "BUFFALO.NY", "NEW.YORK.NY", "ROCHESTER.NY")
    }
        selectInput("cor_locs", "Location", c("ALL", cor_locs), multiple = TRUE, selected = d_sel)
  })  
    

    #correlation plot
    output$cor<- renderPlot({
       # disease_name <- "SMALLPOX"
        disease_name <- input$diseaseds
        df <- diseases[[disease_name]]
        #subset df to proper time range
        print(input$timeds)
        df <- subset(df, df$date >= input$timeds[1] & df$date <= input$timeds[2])      
        #print(head(df))
       # print(tail(df))
        if("ALL" %in% input$cor_locs){
            locs <- colnames(df)[-c(1,2, ncol(df))]
          } else {
            locs <- input$cor_locs
          }
        print(locs)

        df <- df[, c("WEEK", "YEAR", locs , "date")]
#        print(head(df))
           
        nms <- colnames(df)
        print(nms)
        mat <- data.matrix(df[, -c(1,2, ncol(df))])
        cormat <- cor(mat, use="pairwise.complete.obs", method="spearman")
        melted_cormat <- melt(cormat)

           #distance and incidence
        if (input$radiods == 2){
          if (input$diseaseds == "DIPHTHERIA"){
              print(locs)
              print(dim(cormat))
               cors <- as.vector((cormat))
            city_inds <- which( city_lookup$NAME_1 %in% locs)
              print(city_inds)
            ct_dist <- city_dist[city_inds, city_inds] #extract chosen states

            dist <- as.vector(t(ct_dist))
           # plot(dist, cors)
           my_df <- data.frame(dist=dist, cors=cors)
            p <- ggplot(my_df, aes(dist, cors)) + geom_point(colour="gold", size=2) +
                geom_smooth(level=.999, colour="blue", fill="blue")+ ggtitle(disease_name) +
                 labs(x="Distance (Scaled)", y="Correlation") +
                theme_minimal()  
            print(p)
              
           # plot(1,1, main="Under Construction")
        } else {
            cors <- as.vector((cormat))
            state_inds <- which( center_df$region %in% locs)
            st_dist <- state_dist[state_inds, state_inds] #extract chosen states

            dist <- as.vector(t(st_dist))
           # plot(dist, cors)
           my_df <- data.frame(dist=dist, cors=cors)
            p <- ggplot(my_df, aes(dist, cors)) + geom_point(colour="gold", size=2) +
                geom_smooth(level=.999, colour="blue", fill="blue")+ ggtitle(paste(disease_name, "\n", input$timeds[1], "-", input$timeds[1])) +
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
                coord_fixed() + ggtitle(paste(disease_name, "\n", input$timeds[1], "-", input$timeds[1]))
            print(g)
        }
    })
}
