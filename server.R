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


bali_data <- fromJSON(file = "data_bali.json")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
  
  output$img2 = renderUI({
    
  sup_data <- bali_data
  sup_vector <- c() 
  sup_lat <- c() 
  sup_lon <- c() 
  
    # Side image output
        
        for (i in 1:length(sup_data)){
            
            for (label in input$labels){ 
                
                if (label %in% unlist(sup_data[[i]]['label'])){
                    
                    sup_vector <- c(sup_vector, names(sup_data)[i])
                    sup_lat <-  c(sup_lat, sup_data[[i]]['latitude'])
                    sup_lon <-  c(sup_lon, sup_data[[i]]['longitude'])

                }
            }
        }
        
        
        for (i in 1:length(sup_data)){
            
            for (tag in input$tags){ 
                
                if (tag %in% unlist(sup_data[[i]]['tags'])){
                    
                    sup_vector <- c(sup_vector, names(sup_data)[i])
                    sup_lat <-  c(sup_lat, sup_data[[i]]['latitude'])
                    sup_lon <-  c(sup_lon, sup_data[[i]]['longitude'])

                }
            }
        }
        
        
        for (i in 1:length(sup_data)){
            
            for (county in input$countys){ 
                
                if (county %in% unlist(sup_data[[i]]['county'])){
                    
                    sup_vector <- c(sup_vector, names(sup_data)[i])
                    sup_lat <-  c(sup_lat, sup_data[[i]]['latitude'])
                    sup_lon <-  c(sup_lon, sup_data[[i]]['longitude'])

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

      sup_data <- bali_data
      sup_vector <- c() 
      sup_lat <- c() 
      sup_lon <- c() 
      
      # Side image output
      
      for (i in 1:length(sup_data)){
          
          for (label in input$labels){ 
              
              if (label %in% unlist(sup_data[[i]]['label'])){
                  
                  sup_vector <- c(sup_vector, names(sup_data)[i])
                  sup_lat <-  c(sup_lat, sup_data[[i]]['latitude'])
                  sup_lon <-  c(sup_lon, sup_data[[i]]['longitude'])
                  
              }
          }
      }
      
      
      for (i in 1:length(sup_data)){
          
          for (tag in input$tags){ 
              
              if (tag %in% unlist(sup_data[[i]]['tags'])){
                  
                  sup_vector <- c(sup_vector, names(sup_data)[i])
                  sup_lat <-  c(sup_lat, sup_data[[i]]['latitude'])
                  sup_lon <-  c(sup_lon, sup_data[[i]]['longitude'])
                  
                  
              }
          }
      }
      
      
      for (i in 1:length(sup_data)){
          
          for (county in input$countys){ 
              
              if (county %in% unlist(sup_data[[i]]['county'])){
                  
                  sup_vector <- c(sup_vector, names(sup_data)[i])
                  sup_lat <-  c(sup_lat, sup_data[[i]]['latitude'])
                  sup_lon <-  c(sup_lon, sup_data[[i]]['longitude'])
                  
              }
          }
      }
      
      
      if (length(sup_lat) > 0) {
          
          df.plot <- data.frame(unlist(sup_vector), unlist(sup_lat), unlist(sup_lon))
          colnames(df.plot) <- c('id', 'lat', 'lon' )
          df.plot$lat <- as.numeric(as.character(df.plot$lat)) 
          df.plot$lon <- as.numeric(as.character(df.plot$lon))
          
    
          leaflet(df.plot) %>%
            addTiles() %>%
            setView(lat = -8.48, lng = 115.1, zoom = 10) %>%
            addCircleMarkers(lng = ~lon, # add marker
                             color = "#03F",
                             lat = ~lat,
                             radius = 1,
                             popup = paste0("<img src = ", sup_vector, ".jpg>")
        )
      }
                         
                         # dashArray = NULL,
                         # weight = 1,
                         #layerId = ~Country,
                         # fillOpacity = 0.3,
                         # radius = 1
                         #radius = ~total_cases_per_million**(1/2)/8)



    })
    
})