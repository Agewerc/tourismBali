########################################
# Student: Alan Gewerc                 #
# StudentID: 29961246                  #
# Data Visalisation - FIT5147          #
# Lab: 02 Online                       #
########################################

####################
####################
# Import Libraries
####################
####################

library(shiny) # to build app
library(shinyWidgets) # to customise app
library(gridExtra) # to make plots arrenged
library(png) # for images
library(grid) # arrange plots
library(leaflet) # to plot maps
library(geojsonio) # to parse geojson
library(rjson) # to build json
library(sf) # for spatial data
library(dplyr) # For Data manipulation
library(wordcloud) # plot wordcloud
library(tm) # text manipulation
library(SnowballC) 
library(RColorBrewer)
library(plotly) # beautiful plots
library(treemap) # treemap
library(ggplot2)  # general plotting
library(wordcloud2)
library(stringr) # text manipulation


####################
####################
# Import Data
####################
####################

bali_data <- fromJSON(file = "data_bali.json") # main json with metadata
df.word <- read.csv("df.word.csv") # for wordcloud
df.country_words <- read.csv("country_words.csv")
df.popular.words <- read.csv("popular_words.csv")
flights <- read.csv('countries_flow.csv')
colnames(df.popular.words) <- c('Label', 'Freq')


####################
####################
# Shiny Server
####################
####################

shinyServer(function(input, output, session) {
  
  
  ##############################################################
  ##############################################################
  # Page/Tab 1
  ##############################################################
  ##############################################################
  
  ##############################################################
  # Display of Images ##########################################
  ##############################################################
  
  output$img2 = renderUI({
    
    #### Select the Images to be displayed - details are placed in the vectors
    sup_data <- bali_data
    sup_vector <- c() 
    sup_lat <- c() 
    sup_lon <- c() 
    
    
    # Images selected according to labels
    #####################################
    for (i in 1:length(sup_data)){
      
      for (label in input$labels){
        
        if (label %in% unlist(sup_data[[i]]['label'])){
          
          sup_vector <- c(sup_vector, names(sup_data)[i])
          sup_lat <-  c(sup_lat, sup_data[[i]]['latitude'])
          sup_lon <-  c(sup_lon, sup_data[[i]]['longitude'])
          
        }
      }
    }

    # Images selected according to tags
    #####################################
    for (i in 1:length(sup_data)){
      
      for (tag in input$tags){
        
        if (tag %in% unlist(sup_data[[i]]['tags'])){
          
          sup_vector <- c(sup_vector, names(sup_data)[i])
          sup_lat <-  c(sup_lat, sup_data[[i]]['latitude'])
          sup_lon <-  c(sup_lon, sup_data[[i]]['longitude'])
        }
      }
    }
    
    
    # Images selected according to region
    #####################################
    for (i in 1:length(sup_data)){
      
      for (county in input$countys){
        
        if (county %in% unlist(sup_data[[i]]['county'])){
          
          sup_vector <- c(sup_vector, names(sup_data)[i])
          sup_lat <-  c(sup_lat, sup_data[[i]]['latitude'])
          sup_lon <-  c(sup_lon, sup_data[[i]]['longitude'])
          
        }
      }
    }


    # After all images are selected they are displayed in the tab 'All Images'
    #####################################
    filename <- lapply(sup_vector, function(id) {paste0(id, '.jpg')})
    imgside <- lapply(filename, function(file){
      
      column(4,
             tags$img(src=file, 
                      width = 470, 
                      height=400, 
                      style = 'border: 2px; border-radius: 10px; ; margin-top:7px')
      )
      
    })
    
    do.call(tagList, imgside)
  })
  
  
  ##############################################################
  # Barplot ####################################################
  ##############################################################

  output$plot <- renderPlotly({
    
    p <- ggplot(data=df.popular.words, aes(x=reorder(Label,Freq), y=Freq, text = paste('Label :', Label, "<br>",
                                                                                       'Frequency: ', Freq))) +
      geom_bar(stat="identity", fill="royalblue2") + coord_flip() + ggtitle('Popular Labels Observed in Images') + 
      theme_bw() + labs(x = "Computer Vision Labels", y = 'Computer Vision Labels') + 
      theme(plot.title = element_text(family="Times", size=26, colour = '#48ca3b'))
    
    p <- ggplotly(p, tooltip = 'text')
    p %>% config(displayModeBar = F)

  })

  
  ##############################################################
  # Leaflet Dot Density Map ####################################
  ##############################################################
  
  output$map <- renderLeaflet({
    
    #### Select the Images to be displayed - details are placed in the vectors
    sup_data <- bali_data
    sup_vector <- c()
    sup_lat <- c()
    sup_lon <- c() 
    

    # Images selected according to labels
    #####################################
    for (i in 1:length(sup_data)){
      for (label in input$labels){
        if (label %in% unlist(sup_data[[i]]['label'])){
          
          sup_vector <- c(sup_vector, names(sup_data)[i])
          sup_lat <-  c(sup_lat, sup_data[[i]]['latitude'])
          sup_lon <-  c(sup_lon, sup_data[[i]]['longitude'])
        }
      }
    }
    
    # Images selected according to tags
    #####################################
    for (i in 1:length(sup_data)){
      for (tag in input$tags){ 
        if (tag %in% unlist(sup_data[[i]]['tags'])){
          
          sup_vector <- c(sup_vector, names(sup_data)[i])
          sup_lat <-  c(sup_lat, sup_data[[i]]['latitude'])
          sup_lon <-  c(sup_lon, sup_data[[i]]['longitude'])
          
        }
      }
    }
    
    # Images selected according to Region
    #####################################
    for (i in 1:length(sup_data)){
      for (county in input$countys){ 
        if (county %in% unlist(sup_data[[i]]['county'])){
          
          sup_vector <- c(sup_vector, names(sup_data)[i])
          sup_lat <-  c(sup_lat, sup_data[[i]]['latitude'])
          sup_lon <-  c(sup_lon, sup_data[[i]]['longitude'])
          
        }
      }
    }
    
    ###############################
    # Plotting
    ###############################
    
    if (length(sup_lat) > 0) {
      
      df.plot <- data.frame(unlist(sup_vector), unlist(sup_lat), unlist(sup_lon))
      colnames(df.plot) <- c('id', 'lat', 'lon' )
      df.plot$lat <- as.numeric(as.character(df.plot$lat)) 
      df.plot$lon <- as.numeric(as.character(df.plot$lon))
      
      
      tag.map.title <- tags$style(HTML("
              .leaflet-control.map-title { 
                left: 10%;
                transform: translate(-50%,20%);
                text-align: center;
                padding-left: 10px; 
                padding-right: 10px; 
                background: rgba(255,255,255,0.75);
                font-weight: bold;
                font-size: 28px;
              }
            "))
      
      title <- tags$div(
        tag.map.title, HTML("Bali Map and Images")
      )  
      
      leaflet(df.plot) %>%
        addTiles() %>%
        setView(lat = -8.48, lng = 115.1, zoom = 10) %>%
        addControl(title, position = "topright", className="map-title") %>%
        addCircleMarkers(lng = ~lon, # add marker
                         color = "#03F",
                         lat = ~lat,
                         radius = 1,
                         popup = paste0("<img src = ", sup_vector, ".jpg width=300 height=300>")
        )
      
      
      ############## If there is no selection make simple plot 
    } else {
      
      leaflet() %>%
        addTiles() %>%
        setView(lat = -8.48, lng = 115.1, zoom = 10)
    }
  })
  

  ##############################################################
  ##############################################################
  # Page 2
  ##############################################################
  ##############################################################
  
  ##############################################################
  # Reponsive Wordcloud to Cloropleth Map ######################
  ##############################################################
  
  observeEvent(input$baliclo_shape_click, {
    region.filter <- input$baliclo_shape_click$id
    if(!(input$baliclo_shape_click$id %in% states$nm_kabkota)){region.filter <- "DENPASAR"}
    
    output$title <- renderText({
      { paste("Popular Labels in",str_to_title(input$baliclo_shape_click$id)) }

    })
  })
  
  observeEvent(input$baliclo_shape_click, {
    region.filter <- input$baliclo_shape_click$id
    if(!(input$baliclo_shape_click$id %in% states$nm_kabkota)){region.filter <- "DENPASAR"}
    
    df.word.plot <- df.word %>% filter(name == region.filter)
    
    
    output$wordcloud <- renderWordcloud2({
      
      demoFreq = df.word.plot
      set.seed(1234)
      wordcloud2(data = demoFreq, minSize = 20, 
                 color='random-dark')
    })
    
  })

  ##############################################################
  # Choropleth  ################################################
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
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = 'Number of Images',
                position = "bottomleft") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap)
  })

  
  ##############################################################
  ##############################################################
  # Page 3
  ##############################################################
  ##############################################################
  
  ##############################################################
  # Flowmap
  ##############################################################
  
  output$flowmap <- renderPlotly({
    
    geo <- list(
      projection = list(type = 'Natural earth'),
      showland = TRUE,
      landcolor = toRGB("LightYellow3"),
      countrycolor = toRGB("gray80"))
    
    fig <- plot_geo(color = I("MediumOrchid4"))
    fig <- fig %>% add_markers(
      data = flights, x = ~start_lon, y = ~start_lat, text = ~Country,
      size = ~count, sizes = c(1, 1500), hoverinfo = "text", alpha = 0.5)
    
    fig <- fig %>% add_segments(
      data = flights,
      x = ~start_lon, xend = ~end_lon,
      y = ~start_lat, yend = ~end_lat,  text = ~Country,
      alpha = 0.3, size = I(1), hoverinfo = "text") %>%
      config(displayModeBar = F)
    
    fig <- fig %>%
      layout(height=800,
             geo = geo, showlegend = FALSE) %>%
      event_register("plotly_click")
    
    fig
  })
  
  ##############################################################
  # Flowmap Sidepanel
  ##############################################################
  
  observeEvent(event_data('plotly_click'),{
    test <- event_data("plotly_click")
    updateSelectInput(session, 'countries',
                      selected=as.character(sort(df.country_words$Country)[test$pointNumber+1]))
  }) # close observeEvent for clicking on the map
  
  observeEvent(input$countries,{
    
    df.words <- df.country_words %>% filter(Country == input$countries)
    word1 = as.character(df.words$Word1)
    word2 = as.character(df.words$Word2)
    word3 = as.character(df.words$Word3)
    word4 = as.character(df.words$Word4)
    word5 = as.character(df.words$Word5)
    county = as.character(df.words$County)
    percentage = df.words$percentage
    
    output$percentage <- renderText({
      paste0(trunc(percentage*100),"%")
    }) # close renderText percentage
    output$pref_region <- renderText({
      county}) # close renderText prefered region
    output$pop_label1 <- renderText({
      word1
    }) # close renderText popular labels
    output$pop_label2 <- renderText({
      word2
    }) # close renderText popular labels
    output$pop_label3 <- renderText({
      word3
    }) # close renderText popular labels
    output$pop_label4 <- renderText({
      word4
    }) # close renderText popular labels
    output$pop_label5 <- renderText({
      word5
    }) # close renderText popular labels
  })  
  

  ##############################################################
  # Treemap
  ##############################################################
  
  output$treemap <- renderPlot({
    
  treemap(flights, index=c("Continent","Country"), vSize="count", 
          type="index",                            # How you color the treemap. type help(treemap) for more info
          palette = "Set1",                        # Select your color palette from the RColorBrewer presets or make your own.
          title="Tourists by Region",                      # Customize your title
          fontsize.title=32,                       # Size of the title
          
  )
  })

  
})