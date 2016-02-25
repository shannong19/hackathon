# Libraries -------------
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(maptools)

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

# rewrite disease to have a date column
diseases <- lapply(diseases, function(df) {
    df$WEEK <- ifelse(df$WEEK < 10, paste0("0", df$WEEK), df$WEEK)
    date <- paste(df$YEAR, df$WEEK, "7", sep="-")
    date <- as.Date(date, "%Y-%U-%u")
    df$date <- date
    return(df)
})
    
# Names for the lookup table 
disease_names <- as.list(names(diseases))
