# Libraries -------------
library(shiny)
library(shinydashboard)
library(leaflet)
library(plyr)
library(dplyr)
library(maptools)
library(ggplot2)
library(reshape2)
library(animation)
library(RColorBrewer)
library(mclust)
#library(Cairo)
#options(shiny.usecairo=TRUE) 

# Choices for drop down menu's 
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

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


#clustering

clust_df <- read.csv("data/clust_decades.csv")
abs <- usa_shape@data[, c("NAME_1", "HASC_1")]
abs <- abs[,-3] # get rid of alaska 2
abs$NAME_1 <- gsub(" ", ".", abs$NAME_1)
abs$HASC_1 <- gsub("US.", "", abs$HASC_1)
j.df <- clust_df[, 1:2]
colnames(j.df)[2] <- "NAME_1"
new_df <- join(j.df, abs, by="NAME_1", match="first")
clust_df$X <- new_df$HASC_1
colnames(clust_df)[1] <- "AB"


## set.seed(42)
## n <- 1000
## cluster_df <- read.csv("data/cluster.csv", stringsAsFactors=FALSE)
## cluster_df$name <- cluster_df$NAME_1
## cluster_df <- cluster_df[, c(-3)]
## #inds <- sample(1:nrow(cluster_df), size=n)
## #clust <- cluster_df[inds,]
## clust <- cluster_df
## clust$date <- as.Date(clust$date, "%Y-%m-%d")
## clust$year <- format(clust$date, "%Y")
## clust$decade <- floor(as.integer(clust$year) / 10) * 10
## clust_df <- clust[, -c(1, (ncol(clust) - 2):ncol(clust))]
## my_clust <- data.frame(name=clust$name, decade=clust$decade, clust_df)

## dd_clust <- ddply(.data=my_clust, .variables=c("name", "decade"), .fun=function(df){
##     sub <- colMeans(df[, -c(1:2)], na.rm=TRUE)
##     sub <- ifelse( is.nan(sub), 0, sub)
##     return(sub)
## })

## center_df$name <- center_df$region
## centers <- center_df[, c(1,2,4)]
## my_df <- join(dd_clust, centers, by="name")
## write.csv(my_df, "data/clust_decades.csv")


#hc <- hclust(dist(

## cluster_list <- lapply(names(diseases)[-1], function(nm){
##     df <- diseases[[nm]]
##     mega_df <-  do.call('rbind', lapply(3:(ncol(df)-1), function(i) {
##           subdf <- df[,c( ncol(df), i)] #date then incidence
##           subdf$NAME_1<- colnames(subdf)[2] #make a state id
##           colnames(subdf)[1:2] <- c("date", nm)
##           return(subdf)
##           }))
##     mega_df[nm] <- as.numeric(mega_df[, nm])
##     return(mega_df)
## })

## cluster_df <- cluster_list[[1]]
## for (i in 2:length(cluster_list)){
##     cluster_df <- join(cluster_df, cluster_list[[i]], by=c("date", "NAME_1"), type="full")
## }

## write.csv(cluster_df, "data/cluster.csv", row.names=FALSE)
