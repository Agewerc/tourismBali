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
library(dplyr)
library(wordcloud)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(plotly)
library(treemap)

bali_data <- fromJSON(file = "data_bali.json")
df.word <- read.csv("df.word.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    
    # read data
    flights <- read.csv('countries_flow.csv')
    bali_data <- fromJSON(file = "data_bali.json")
    

    ##############################################################
    ##############################################################
    # Page 3
    ##############################################################
    ##############################################################
    
    # Treemap
    ##############################################################
    
    output$treemap <- renderPlot({
      
        treemap(dtf = flights, vSize = count, index=c("Continent","Country"))
            
    })
      
    # Flowmap
    ##############################################################
          
    output$flowmap <- renderPlotly({
      
      geo <- list(
        projection = list(type = 'Natural earth'),
        showland = TRUE,
        landcolor = toRGB("gray95"),
        countrycolor = toRGB("gray80"))
      
      fig <- plot_geo(color = I("red"))
      fig <- fig %>% add_markers(
          data = flights, x = ~start_lon, y = ~start_lat, text = ~Country,
          size = ~count2, sizes = c(1, 300), hoverinfo = "text", alpha = 0.5)
      
      fig <- fig %>% add_segments(
          data = flights,
          x = ~start_lon, xend = ~end_lon,
          y = ~start_lat, yend = ~end_lat,  text = ~Country,
          alpha = 0.3, size = I(1), hoverinfo = "text") %>%
          config(displayModeBar = F)
      
      fig <- fig %>% layout(height=800,
          title = 'Feb. 2011 American Airline flight paths<br>(Hover for airport names)',
          geo = geo, showlegend = FALSE)

      fig
      })
    
    
    ##############################################################
    ##############################################################
    # Page 2
    ##############################################################
    ##############################################################
    
  
    # Wordcloud  #################################################
    ##############################################################

    observeEvent(input$baliclo_shape_click, {
        region.filter <- input$baliclo_shape_click$id
        if(!(input$baliclo_shape_click$id %in% states$nm_kabkota)){region.filter <- "DENPASAR"}
      
        df.word.plot <- df.word %>% filter(name == region.filter)
      
        output$wordcloud <- renderWordcloud2({
        
            demoFreq = df.word.plot
            set.seed(1234)
            wordcloud2(data = demoFreq, minSize = 20, color='random-dark', shape = 'diamond')
        })

    })
    
    
    # Cloropleth  ################################################
    ##############################################################
    
    states <- geojsonio::geojson_read("bali.geojson", what = "sp")
    bins <- c(0, 500, 1000, 1500, 2000, 2500, 3000, Inf)
    pal <- colorBin("YlGnBu", domain = states$density, bins = bins)
    
    output$baliclo <- renderLeaflet({
      
        labels <- sprintf(
            "<strong>%s</strong><br/>%g Images",
            states$nm_kabkota, states$density) %>%
            lapply(htmltools::HTML)
        
        m <- leaflet(states) %>%
            setView(lat = -8.48, lng = 115.1, zoom = 10) %>%
            addTiles %>%
            addPolygons(
                layerId= ~nm_kabkota,
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

    
    ##############################################################
    ##############################################################
    # Page 1
    ##############################################################
    ##############################################################
    
    # Display Images #############################################
    ##############################################################

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
    
    
    # Leaflet ####################################################
    ##############################################################
    
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
      } else {
          
          leaflet() %>%
              addTiles() %>%
              setView(lat = -8.48, lng = 115.1, zoom = 10)
      }
    })
})