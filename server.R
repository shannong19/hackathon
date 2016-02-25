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



    output$avail_locs <- renderUI({
        disease_index <- which(names(x = diseases) == input$diseaseds)
        current_disease <- diseases[[disease_index]]    
        avail_locs <- as.list(colnames(current_disease))
        selectInput("avail_locs", "Location 2", avail_locs)
        disease_name <- names(diseases)[disease_index]
      })  

    output$radiods <- renderUI({

    })

    

    #correlation plot
    output$cor<- renderPlot({
       # disease_name <- "SMALLPOX"
        disease_name <- input$diseaseds
        df <- diseases[[disease_name]]
        #subset df to proper time range
        print(input$timeds)
        subset(df, df$date >= input$timeds[1] & df$date <= input$timeds[2])
        print(head(df))
        print(tail(df))
        
        nms <- colnames(df)
        melt_names <- nms[!(nms %in% c("YEAR", "WEEK", "date"))]
        mat <- data.matrix(df[, -c(1,2, ncol(df))])
        cormat <- cor(mat, use="pairwise.complete.obs", method="spearman")
       # print(cormat)
        melted_cormat <- melt(cormat)
        
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
    })
  
  
  
}
