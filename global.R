# Libraries -------------
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(maptools)
library(ggplot2)
library(reshape2)
library(plyr)
#library(Cairo)
#options(shiny.usecairo=TRUE) 



# Loading in our data ---------
load(file = "data/disease_list.Rda")
city_lookup <- read.csv("data/city_table.csv", stringsAsFactors = FALSE)
usa_shape <- readShapeSpatial(fn = "data/USA_adm_shp/USA_adm1.shp")
load(file = "data/us_fortify.Rda")
usa_shape@data$NAME_1 <- toupper(as.character(usa_shape@data$NAME_1))
state_pops <- read.csv("data/state_pops_1790-2010.csv")

# rewrite disease to have a date column
diseases <- lapply(diseases, function(df){
    df$WEEK <- ifelse(df$WEEK < 10, paste0("0", df$WEEK), df$WEEK)
    date <- paste(df$YEAR, df$WEEK, "7", sep="-")
    date <- as.Date(date, "%Y-%U-%u")
    df$date <- date
    return(df)
})
    
# Names for the lookup table 
disease_names <- as.list(names(diseases))
disease_names_nodip <- disease_names[-1]

# Centroids
center_df <- as.data.frame(coordinates(usa_shape))
names(center_df) <- c("Longitude", "Latitude")
center_df$region <- gsub(" ", ".", toupper(usa_shape$NAME_1))
center_df <- center_df[-2,] # there were two alaskas

# combine lat/long with
mat <- data.matrix(center_df[,1:2])

state_dist <- apply(mat, 1, function(x) {
    apply(mat, 1, function(y) sqrt(sum(x-y)^2))
})
    
## state pops
state_pops$NAME_1<- gsub(" ", ".", toupper(state_pops$Name))


#city numaes
city_lookup$NAME_1 <- gsub(" ", ".", toupper(city_lookup$cs))
city_lookup <- city_lookup[, c("lng", "lat", "NAME_1")]
colnames(city_lookup)[1:2] <- c("Longitude", "Latitude")
city_lookup$NAME_1[73] <- "SYERACUSE.NY"
city_lookup$NAME_1[85] <- "PITTSBURGH.PA"


city_mat <- data.matrix(city_lookup[,1:2])
city_dist <- apply(city_mat, 1, function(x) {
    apply(city_mat, 1, function(y) sqrt(sum(x-y)^2))
})
