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

library(shiny)
library(leaflet)
library(rjson)
library(shinydashboard)
library(wordcloud2)
library(plotly)
library(shinythemes)

####################
####################
# Import Data
####################
####################

label_vector <- readRDS("labels.Rds")
tag_vector <- readRDS("tags.Rds")
county_vector <- readRDS("countys.Rds")
country_vector <- readRDS("countrys.rds")
df.country_words <- read.csv("country_words.csv")

####################
####################
# CSS for a tab
####################
####################

myTitlePanel <- function (title, windowTitle = title) {
  css <- paste(
    "padding-left: 15px",
    "margin-left: -15px",
    "margin-right: -15px",
    sep = ";")
  
  tagList(tags$head(tags$title(windowTitle)), 
          h1(title, style = css))
}


####################
####################
# Ui
####################
####################

shinyUI(fluidPage(theme = shinytheme("cerulean"),
    
    # Navbar structure
    navbarPage("Bali Tourism",
        
               
               
               ######################################################################## 
               ############ tab1
               ######################################################################## 

               tabPanel("Attractions",

                        fluidRow(
                          
                          column(6,
                                 tags$head(
                                 tags$style(HTML("
                                 @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                                 h1 {
                                 font-family: 'Garamond';
                                 font-weight: 400;
                                 line-height: 1.1;
                                 color: #48ca3b;
                                 }
                                                 "))
                                 ),
                                 
                                 headerPanel('Choose Labels, #Tags and Regions'),
                                 ),
                          
                          column(2, selectInput("labels",label="Computer Vision Labels ",multiple = TRUE,
                                                choices=label_vector,
                                                selected =c('Beach', 'Temple', 'Nature'))),
                          
                          column(2, selectInput("tags",label="#Tags from Social Media Post",multiple = TRUE, 
                                                  choices=tag_vector, selected = c('beautiful'))),
                          
                          column(2, selectInput("countys",label="Region",multiple = TRUE, 
                                                  choices=county_vector, 
                                                  selected =c('Buleleng Kabupaten')))
                          ),
                        
                        ############ Two tabs in tab 1
                        tabsetPanel(type = "tabs",
                                    tabPanel("Bali Map",
                                    sidebarPanel(style = "margin-top: 50px; 
                                                 background-color:transparent;
                                                 border:none;",
                                                 width = 2,
                                                 tags$h4("Visualise Images by Clicking on the Markers"),
                                                 tags$br(),
                                                 tags$b("The markers represent the exact location where the photo was taken"),
                                                 tags$br(),
                                                 tags$b("You can choose the content of images according to the input options on the top-right"),
                                                 tags$br(),
                                                 tags$br(),
                                                 tags$b("Computer Vision Labels:"),
                                                 "Labels extracted from images using computer vision (Google Vision).",
                                                 tags$br(),
                                                 tags$br(),
                                                 tags$b("#Tags from Social Media Post:"),
                                                 "Hashtags posted from social media users with photos",
                                                 tags$br(),
                                                 tags$br(),
                                                 tags$b("Region:"),
                                                 "The region where the photo was taken according to the location of the tagged image.",
                                                 tags$br(),
                                                 tags$br(),
                                                 tags$b("Go to ", tags$b("Regions/Tourists Origin", style = "color:red"), "to find out more about Tourism in Bali."),
                                                 tags$br(),
                                                 tags$br(),
                                                 tags$br(),
                                                 tags$br(),
                                                 tags$br(),
                                                 tags$br(),
                                                 tags$br(),
                                                 tags$br(),
                                                 tags$br(),
                                                 tags$br(),
                                                 tags$h4("Most Popular Labels From Bali Photos"),
                                                 tags$br(),
                                                 tags$br(),
                                                 "Bali is a Tourism Destination famous for its beaches, beautiful nature landscapes and Hindu Culture.
                                                 We note from the barplot that most labels relate to those topics.",
                                                 tags$br(),
                                                 tags$br(),
                                                 "You can use the information from this plot as inspiration to select labels in 
                                                 the label input of the map above.  Some aditional labels worth exploring 
                                                 on the map are 'Birds, Primate, Surf, Fish, Sunset and much more. "
                                                 ), # end of sideBarPanel

                                    mainPanel(
                                             leafletOutput("map", width = 1600, height = 700),
                                             tags$br(),
                                             tags$br(),
                                             tags$br(),
                                             tags$br(),
                                             plotlyOutput('plot', width = 1400,  height = 600)
                                             ),
                                    ),
                                    
                                    tabPanel("All Images",
                                             uiOutput("img2")
                                             )
                                    ),
               ),

               
               ######################################################################## 
               ############ tab2
               ######################################################################## 

                              
               tabPanel("Regions",
                        
                        fluidRow(
                            tags$head(
                                tags$style(HTML("
                                    h1 {
                                    font-family: 'Garamond'
                                    font-weight: 500;
                                    line-height: 1.1;
                                    color: #48ca3b;
                                    }
                                                "))
                                ),
                          
                          headerPanel("Concentration of Images by Region  - Click to see what social media posts show from each Balinese region!")
                          
                        ),
                        
                        fluidRow(
                          tags$head(
                            tags$style(HTML("
                                    #title {
                                    font-family: 'Garamond';
                                    font-weight: 500;
                                    font-size: 35px;
                                    background-color:transparent;
                                    text-align: center;
                                    border:none;
                                    line-height: 1.1;
                                    color: #48ca3b;
                                    }
                                                "))
                          ),
                          column(8,
                                 leafletOutput("baliclo", height = 800, width = 950)
                                 ),
                          column(4,
                                 verbatimTextOutput('title'),
                                 wordcloud2Output('wordcloud', height = 500, width = 500),
                                 
                                 )
                          ),
                        
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        
                        tags$b(style = 'font-family: Garamond; color: #48ca3b; font-size: 20px',
                        'We can see from the map above that the most appealing regions for tourists are those 
                        closer to the south/center of the Island. Bali has nine regencies, however more than 
                        80% of photos were taken in Badung, Denpasar and Gianyar.'),
                        
                        tags$br(),
                        tags$br(),
                        
                        tags$b(style = 'font-family: Garamond; color: #48ca3b; font-size: 20px',
                        'Denpasar is the capital city of Bali where most tourists stay during their visit in Bali.
                        This is why labels associated with Denpassar are less associated to nature and more urban life.  
                        During the day usually tourists take excursions to the other regions, such as Gianyar, where
                        is the village of Ubud, the most popular place in Bali for Tourism. Young people go a lot also
                        to Badung, where there is more active nightlife, for instance in 
                        the neighboorhoods of Kuta and Seminiak.'),
                        
                        tags$br(),
                        tags$br(),
                        
                        tags$b(style = 'font-family: Garamond; color: #48ca3b; font-size: 20px',
                        'Tourism in Bali is associated specially with nature, beaches and Hindu culture. 
                        Regions that have more attractions related to these topics are the ones with more tourists.
                        If we see the dot-map on the other tab, with all dots (all regions), it is possible to see 
                        that areas close to the beaches are the ones where more photos were taken.'),

               ),
               
               
               ######################################################################## 
               ############ tab3
               ######################################################################## 
               
               tabPanel("Tourists Origin",
                        fluidPage(
                          column(12, offset = 4,
                        myTitlePanel("Flow of Tourists Around the World to Bali"))),
                        
                          sidebarPanel(style = "background: #E6DFE5; margin-top: 35px;",
                                       
                                       tags$b("The size of Circles is representative of the proportion of Tourists 
                                              from the Country. Click on the circles to know more."), 
                                       tags$br(),
                                       tags$br(),
                                       tags$b("You can select a country in the input box or by 
                                              clicking in the markers on the flow map to see some
                                              extra information about preferences according to the origin
                                              of the tourist"), 
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),

                                       selectInput("countries",
                                                   label="Select Country ", 
                                                   multiple = FALSE,
                                                   choices=country_vector,
                                                   selected =c('Australia')),                                       
                                       
                                       width = 3,
                                       tags$b("Preferred Region of Tourists: "), 
                                       tags$br(),
                                       h3(textOutput(outputId = "pref_region")),
                                       tags$br(),
                                       tags$b("Popular Labels Among Tourists: "), 
                                       tags$br(),
                                       h3(textOutput(outputId = "pop_label1")),
                                       h3(textOutput(outputId = "pop_label2")),
                                       h3(textOutput(outputId = "pop_label3")),
                                       h3(textOutput(outputId = "pop_label4")),
                                       h3(textOutput(outputId = "pop_label5")),
                                       tags$br(),
                                       tags$b("% of Visitors who Come From "),
                                       #tags$input(id = 'countries'),
                                       tags$b(" :"),
                                       h3(textOutput(outputId = "percentage")),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$b(
                                         "The Treemap on the write side allows a more 
                                       direct comparation visually. We devide not only by countries but also
                                       continents. We see that the most relevant continent in terms of tourists
                                       to Bali is Europe, followed by Asia"),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$b(
                                       "Plots in this page were made with a sample of +500 unique tourists from over
                                       45 countries."),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$b(
                                        "It is important to mention that our goal in here is not make a rank of the first, 
                                        second, third, ..., more important countries to Bali. Instead our goal is to identify
                                        a group of countries and region that are relevant for Balinese Tourism. 
                                        "),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br()
                                       ), 
                        
                        mainPanel(
                          
                        plotlyOutput(outputId = "flowmap", height = 900),
                        plotOutput(outputId = "treemap", height = 650)
                        
                        
                        )
                        ),

               ######################################################################## 
               ############ tab4
               ######################################################################## 
               
               tabPanel("About",

                        fluidRow(
                          
                          column(width = 4,  align="center",
                                 br(),
                                 br(),
                                 img(src='Bali.PNG', height = '300px', width = '250px', class="circular--square", style="border-radius:50%"),
                          ),
                          
                          column(4, align="center",
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 img(src='monash.jpg', height = '170px', width = '400px'),
                          ),
                          
                          column(4, align="center",
                                 br(),
                                 br(),
                                 img(src='IMG_2609.jpg', height = '300px', width = '250px', class="circular--square", style="border-radius:50%"),
                          )
                        ),
                        fluidRow(
                          column(4,
                                 br(),
                                 div(style = "padding: 15px 15px 15px 15px; text-align: justify; height:300px;width:100%;background-color: #F0F8FF;border-style: none;border-color: #000000",
                                     tags$h4("
                                     Bali is a major world tourism hotspot, having received more than 8.2 million 
                                     travellers in 2019. The Island was also the 19th most visited region in the 
                                     world. The area is heavily dependent on the tourism industry, which 
                                     accounts for more than 30% of its GDP. Given the dependence of the 
                                     local economy with Tourism, local authorities need to excel in managing 
                                     their industry, promoting the region worldwide. The current project can be 
                                     regarded as a consultancy project for the Balinese Tourism Council. 
                                     Firtly, It can be used as tool by tourists worldwide to plan their 
                                     trips to Bali according to what activities they wan't to do. Secondly, 
                                     It can be used by the Balinese government as a business inteligence tool, 
                                     showing what attracts Tourists to Bali and which areas are 
                                     more popular among tourists."))
                          ),
                          
                          column(4,
                                 br(),
                                 div(style = "padding: 15px 15px 15px 15px; text-align: justify; height:300px;width:100%;background-color: #F0F8FF;border-style: none;border-color: #000000",
                                     tags$h4("
                                 The present project was developed for the unit Data Visualisation - FIT5147 from the Monash Faculty of Information Technology. 
                                 The unit is offered for students from the Master of Data Science or any as optinal unit for students from other courses.", br(), 
                                             "Lecturer 1: Sarah Goodwin", br(),
                                             "Lecturer 2: Shirin Ghaffarian Maghool" , br(),
                                             "Tutor1: Farah Tasnuba Kabir", br(),
                                             "Tutor2: Jie (Lewis) Liu"
                                             ))
                          ),
                          
                          column(4,
                                 br(),
                                 div(style = "padding: 15px 15px 15px 15px; text-align: justify; height:300px;width:100%;background-color: #F0F8FF;border-style: none;border-color: #000000",
                                     tags$h4("
                                         Alan Gewerc is a Master of Data Science student at Monash University. He has also experience with business, finance and 
                                         analytics. He is currently developing a Master Thesis about Forecasting with Neural Networks with applications in the 
                                         Medical field. Find more about Alan at:", br(),br(),br(),
                                             "website:     www.alangewerc.com.", br(), 
                                             "linkedin:    linkedin.com/in/alan-gewerc. 
                                         "))
                        )
                        )
               )
    )
    )
)
