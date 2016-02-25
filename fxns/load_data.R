#Load in the data files in data
#SKG
# 2/24/16

#files <- list.files("data")


load_disease <- function(filenames){
    base_names <- basename(filenames)
    diseases <- sub("_.*", rep="", base_names)
    disease_list <- lapply(filenames, function(filename){
        return(read.csv(filename, skip=2))
    })
    names(disease_list) <- diseases
    return(disease_list)
}

#diseases <- load_disease(filenames)
#saveRDS(diseases, file="disease_list.RData")
#test <- readRDS("disease_list.RData")
