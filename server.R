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
library(geojsonio)
library(sf)

bali_data <- fromJSON(file = "data_bali.json")



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    bali_data <- fromJSON(file = "data_bali.json")
    states <- geojsonio::geojson_read("bali.geojson", what = "sp")
    bins <- c(0, 500, 1000, 1500, 2000, 2500, 3000, Inf)
    pal <- colorBin("YlGnBu", domain = states$density, bins = bins)
    
    output$baliclo <- renderLeaflet({
        
        labels <- sprintf(
            "<strong>%s</strong><br/>%g Images",
            states$nm_kabkota, states$density
        ) %>% lapply(htmltools::HTML)
        

        m <- leaflet(states) %>%
            setView(lat = -8.48, lng = 115.1, zoom = 10) %>%
            addTiles %>% 
            addPolygons(
                fillColor = ~pal(density),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
            addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                      position = "bottomleft") %>%
            addProviderTiles(providers$Esri.NatGeoWorldMap)

        })
    
    
    observe({ 
        event <- input$baliclo_shape_click
        output$cnty <- renderText(shape$NAME[shape$CNTY_ID == event$id])
        
    })
    
        
        
  
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
                             popup = paste0("<img src = ", sup_vector, ".jpg width=300 height=300>")
        )
      }else {
          
          leaflet() %>%
              addTiles() %>%
              setView(lat = -8.48, lng = 115.1, zoom = 10)
      }
                         
                         # dashArray = NULL,
                         # weight = 1,
                         #layerId = ~Country,
                         # fillOpacity = 0.3,
                         # radius = 1
                         #radius = ~total_cases_per_million**(1/2)/8)



    })
    
})