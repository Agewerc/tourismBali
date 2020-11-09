#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(shinyWidgets)
library(gridExtra)
library(png)
library(grid)
library(leaflet)


bali_data <- fromJSON(file = "treated_bali_data.json")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # Side image output
    output$img2 = renderUI({
        
        sup_data <- bali_data
        sup_vector <- c() 
        
        for (i in 1:length(sup_data)){
            
            for (label in input$labels){ 

                if (label %in% unlist(sup_data[[i]]['label'])){
                    
                    sup_vector <- c(sup_vector, names(sup_data)[i])

                }
            }
        }
        
        filename <- lapply(sup_vector, function(id) {paste0(id, '.jpg')})
        imgside <- lapply(filename, function(file){
                tags$img(src=file, width=450, height=400)
            })
        
            do.call(tagList, imgside)

    })
    

    output$map <- renderLeaflet({
        leaflet() %>% addTiles() %>% setView(lat = -8.5, lng = 115.15, zoom = 10)
        })
    
    

    
    
    
})