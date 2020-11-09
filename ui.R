#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


label_vector <- readRDS("labels.rds")
tag_vector <- readRDS("tags.rds")
county_vector <- readRDS("countys.rds")




# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    sidebarPanel(),
    
    mainPanel(

    # Application title
    titlePanel("Bali Tourism"),

    # Sidebar with a slider input for number of bins
    
    selectInput(
        inputId = 'country_compare',
        label = 'Select One or More Labels',
        choices = label_vector,
        multiple = TRUE,
        selected = c('United States', 'Brazil', 'India')
        ),
    
    
    img(src = "https://media.timeout.com/images/105240189/image.jpg", height = 72, width = 72)
    
    )

        )
    )
