#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

length(label_vector)
label_vector <- readRDS("labels.rds")
tag_vector <- readRDS("tags.rds")
county_vector <- readRDS("countys.rds")
image_list <- c('5852495442.jpg', '15550188123.jpg', '15582001714.jpg', '15581992784.jpg')



# Define UI for application that draws a histogram
shinyUI(fluidPage(

    
    navbarPage("Bali Tourism",

               tabPanel("Places and Photos",
                        

                        ########################################################
                        ################### input labels, tags and county
                        ########################################################
                        
                        
                        fluidRow(
                            column(2, selectInput("labels",label="Choose Label",multiple = TRUE, 
                                                                         choices=label_vector)), 
                                   

                            column(2, selectInput("tags",label="Choose Tag",multiple = TRUE, 
                                                  choices=tag_vector)), 
                            
                            
                            column(2, selectInput("countys",label="Choose County",multiple = TRUE, 
                                                  choices=county_vector))
                            ),
                        
                        tabsetPanel(type = "tabs",
                                    
                                    tabPanel("Map"),

                           
                                    tabPanel("Images",
                                             
                                             lapply(image_list, FUN = function(i) {
                                                 
                                                 img(src = paste0(i), height = 400, width = 460) }
                                             )
                                             )
                                    )),

               tabPanel("Regions"),
               tabPanel("Tourits Origin"),
               tabPanel("About")
    

               
    )
    )
)



