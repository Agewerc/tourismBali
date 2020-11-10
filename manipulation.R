
# Load the package required to read JSON files.
library("rjson")
library('stringr')
library('jsonlite')

setwd('C:/Users/hp/OneDrive/Monash/FIT5147 - Data Visualisation/Semester - Project/Shiny App/tourismBali')

# Give the input file name to the function.
result <- fromJSON(file = "treated_bali_data.json")
labels <- fromJSON(file = "google_vision_label.json")

length(labels)


files <- list.files('www/')
files <- unlist(lapply(files, str_replace, pattern = '.jpg', replacement = ''))
length(files)

new_result <- list()


for (i in 1:length(result)){
  
  if (names(result)[i] %in% files) {

    x <- result[i][-1]
    x[-1]
    
    x[[1]] <- NULL
    
    
    
    
    
    x <- x[names(x[[1]]) != "url"]
    x <- x[names(x[[1]]) != "favs"]
    x <- x[names(x[[1]]) != "views"]
    x <- x[names(x[[1]]) != "owner_location"]
    x <- x[names(x[[1]]) != "owner_id"]
    x <- x[names(x[[1]]) != "title"]
    x <- x[names(x[[1]]) != "description"]

    new_result[names(result)[i]] <- x
    
  }
}




length(new_result)
new_result[1]




jsonData <- toJSON(new_result)
write(jsonData, "bali_data.json")


test <- fromJSON(file = "bali_data.json")
'48220063301' %in% names(test)

