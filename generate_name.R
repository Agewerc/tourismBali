


# Load the package required to read JSON files.
library("rjson")
library('stringr')

# Give the input file name to the function.
result <- fromJSON(file = "data_bali.json")

vector_labels <- c()
vector_tags <- c()
vector_countys <- c()
vector_imageid <- names(result)


for (i in 1:length(result)){
  
  vector_labels <- c(vector_labels, result[[i]][['label']] )
  vector_tags <- c(vector_tags, result[[i]][['tags']] )
  vector_countys <- c(vector_countys, result[[i]][['county']] )
  
}

vector_tags <- str_remove(vector_tags, pattern = "foursquare.*")
vector_tags <- vector_tags[vector_tags != ""]


vector_labels <- unique(vector_labels)
vector_tags <- unique(vector_tags)
vector_countys <- unique(vector_countys)


saveRDS(vector_labels, file = "labels.Rds")
saveRDS(vector_tags, file = "tags.Rds")
saveRDS(vector_countys, file = "countys.Rds")
