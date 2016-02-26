current_disease <- diseases[[4]]

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
      usa_map + geom_point(size = 10) + ggtitle(paste0("Year: ", year)) +
      scale_fill_distiller(palette = "Spectral")
    print(year_plot)    
  }
}, movie.name = "test.gif", interval = 1)

