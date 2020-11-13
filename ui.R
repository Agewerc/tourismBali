#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(leaflet)
library(rjson)
library(shinydashboard)
library(wordcloud2)
library(plotly)


label_vector <- readRDS("labels.rds")
tag_vector <- readRDS("tags.rds")
county_vector <- readRDS("countys.rds")



# Define UI for application that draws a histogram
shinyUI(fluidPage(

    
    navbarPage("Bali Tourism",

               tabPanel("Places and Photos",
                        

                        ########################################################
                        ################### input labels, tags and county
                        ########################################################
                        
                        fluidRow(
                            column(2, selectInput("labels",label="Choose Label",multiple = TRUE, 
                                                                         choices=label_vector, selected ='Beach')), 
                                   
                            column(2, selectInput("tags",label="Choose Tag",multiple = TRUE, 
                                                  choices=tag_vector)), 

                            column(2, selectInput("countys",label="Choose County",multiple = TRUE, 
                                                  choices=county_vector))
                            ),
                        
                        tabsetPanel(type = "tabs",
                                    
                                    tabPanel("Map",
                                    
                                    
                                    leafletOutput("map", height = 700)     
                                    
                                    ),

                           
                                    tabPanel("Images",
                                             
                                             uiOutput("img2")
                                             )
                                    ),
               ),

               tabPanel("Regions", 
                        fluidRow(
                          column(7,
                                 leafletOutput("baliclo", height = 700, width = 1000)
                                 ),
                          column(5,
                                 wordcloud2Output('wordcloud')
                                 # plotOutput('wordcloud')
                                 )
                          )
               ),
               
               tabPanel("Tourits Origin",
                        
                        fluidRow(
                            column(7,
                            plotlyOutput(outputId = "flowmap")
                            ), 
                            column(7,
                                   plotlyOutput(outputId = "treemap")
                            )
                            )
                        ),

               tabPanel("About")
    )
    )
)
