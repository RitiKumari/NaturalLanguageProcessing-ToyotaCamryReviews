
######################################
## Author:   Riti Kumari (sinhariti61@gmail.com)
## Date:     2018-04-02
## Title:    Natural Language Processing 
## Purpose:  Analysis of Toyota Camry reviews for the year 2012-2016
#            Train the model and predict the output rating for the year 2017
#            Categorized the reviews as per the tags (Service, Handling, Price , Interior) 
#            Calculated the sentiment score (AFINN Score) for each review
#            Visualized the word for the highest tf-idf score for each tag using ggplot2
######################################

library(shiny)
library(rvest)
library(tm)
library(stringr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(caret)
library(DT)

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
                 #selectInput("Year", "Choose the Review Year:", choices = c('2016','2015','2014','2013','2012')),
                 img(src="Shiny-image.png", height=100, width=200), 
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
                             We are specifically interested in the 2012 - 2017 Camry model. We will be using the
                             2012, 2013, 2014, 2015 and 2016 reviews for training and the 2017 reviews for testing
                             purposes.")
                  ),
                  tabPanel("Data",
                           fluidRow
                           (
                             br(),
                             column(12, h3("Training Data"), dataTableOutput("trainData")),
                             column(12, h3("Test Data for Year 2017"), dataTableOutput("testData"))
                           )),
                  navbarMenu( "Computation" ,
                              tabPanel("Train Data",
                                       #For making grid
                                       fluidRow(
                                         br(),
                                         column(12, h3("Computated Train Data"), dataTableOutput("computedTrain"))
                                       )
                              ),
                              tabPanel("Test Data",
                                       fluidRow(column(12,h3("Computed Test Data"), dataTableOutput("computedTest"))),
                                       fluidRow(column(4, h3("Confusion Matrix"), verbatimTextOutput("confmatrix"))),
                                       fluidRow(column(4, h3("Accuracy"), verbatimTextOutput("accuracy")))
                                       )
                  ),
                  tabPanel("Rating Comparison",
                           fluidRow
                           (
                             br(),
                             column(12, h3("Average Rating Comparison"), dataTableOutput("sentiment"))
                           )),
                  tabPanel("TF-IDF",
                           fluidRow
                           (
                             br(),
                             column(12, h3("TF-IDF for Train Data"), dataTableOutput("tfidf")),
                             column(12, h3("Plot"), plotOutput("tfPlot"))
                           )),
                  tabPanel("Comments",
                           h3("Insights & Comments"),
                           p("1.> The packages used are shiny, rvest, tm, stringr, tidytext, dplyr, ggplot2, caret and DT."),
                           p("2.> The model is trained using linear modelling with Year 2012-2016 camry data."),
                           p("3.> There are a few comments that are very short and is either very positive or 
                             very negative and hence have less sentiment score but the user rating is very high or very less."),
                           p("4.> The accuracy of the model is approximately 67 %.")
                           )
      ),
      h5(strong(tags$footer("@ Shiny App", align ="center")))
    )
  )
  
  
))

