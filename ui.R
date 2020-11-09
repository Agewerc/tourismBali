#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Bali Tourism"),

    # Sidebar with a slider input for number of bins
    
    selectInput(
        inputId = 'country_compare',
        label = 'Select One or More Countries',
        choices = country_vector,
        multiple = TRUE,
        selected = c('United States', 'Brazil', 'India')
        ),
    
        )
    )
