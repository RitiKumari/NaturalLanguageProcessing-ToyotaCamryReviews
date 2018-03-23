library(shiny)
library(rvest)
library(XML)
library(tm)
library(stringr)
library(tidytext)

shinyUI(fluidPage(
  
  #titlePanel
  titlePanel("RitiKumari_Natural_Language_Processing"),
  br(),
  #sidebarLayout
  sidebarLayout
  (
    sidebarPanel(tags$b("The reviews are taken from :"), 
                 br(),
                 tags$a(href = "https://www.cars.com/research/toyota-camry/", "Toyota Camry Reviews"),
                 br(),
                 br(),
                 selectInput("Year", "Choose the Review Year:", choices = c('2016','2015','2014','2013','2012')),
                 img(src="Shiny-image.png", height=100, width=250), 
                 width=2
    ),
    #mainPanel
    mainPanel
    (
      
      tabsetPanel(type="tab",
                  tabPanel("Preface",
                           tags$style("body{background-color:Ivory ; color:brown}"),
                           br(),
                           h2("Natural Language Processing"),
                           br(),
                           p("This project is to perform some tasks that are commonly used in
                            Natural Language Processing. "),
                           br(),
                           p("We are analyzing Toyota Camry car reviews for this project. The reviews are
                             available online and it is being downloaded programmatically from
                             https://www.cars.com/research/toyota-camry/ .
                             We are specifically interested in the 2012 - 2017 Camry model. You will be using the
                             2012, 2013, 2014, 2015 and 2016 reviews for training and the 2017 reviews for testing
                             purposes.")
                           ),
                  tabPanel("Train Data",
                           fluidRow
                           (
                             br(),
                             column(12, h3("Training Data"), dataTableOutput("trainData"))
                           )),
                  tabPanel("Test Data",
                           fluidRow
                           (
                             br(),
                             column(12, h3("Test Data for Year 2017"), dataTableOutput("testData"))
                           )),
                  navbarMenu( "Sentiment-Comparison" ,
                              tabPanel("Sentiment-Comparison",
                                       #For making grid
                                       fluidRow(
                                         br(),
                                         column(4, h3("Average Sentiment Rating"), dataTableOutput("avgSentiScore")),
                                         column(4, h3("Average Review Rating"), dataTableOutput("receiver"))
                                       )
                              ),
                              tabPanel("TagsScore-Comparison",
                                       fluidRow(column(4,h3("Average Sentiment Rating"), dataTableOutput("department"))))
                  ),
                  tabPanel("TF-IDF",
                           fluidRow
                           (
                             br(),
                             column(12, h3("Test Data for Year 2017"), dataTableOutput("tfidf"))
                           ))
                ),
      h5(strong(tags$footer("@ Shiny App", align ="center")))
      )
  )
  
  
))

