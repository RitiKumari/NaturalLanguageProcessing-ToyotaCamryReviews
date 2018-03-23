library(shiny)
library(rvest)
library(XML)
library(tm)
library(stringr)
library(tidytext)
library(dplyr)


shinyServer(function(input,output)
{
  ######### Web Scrapping #######
  webSrapping <- function(url, year){
    web <- read_html(url)

     title_html <- html_nodes(web,'.cui-zeta.mmy-consumer-reviews__title span')
     title <- html_text(title_html)

     body_html <- html_nodes(web,'.mmy-reviews__blurb span')
     body <- html_text(body_html)

     rating_html <- html_nodes(web, '.cr-star-rating')
     rating <- html_attr(rating_html, "rating")

     title <- as.data.frame(title)
     body <- as.data.frame(body)
     rating <- as.data.frame(rating)
     year<- as.data.frame(year)

     tempdata <- cbind.data.frame(title,body)
     colnames(tempdata) <- c("Title","Body")
     data2 <- paste(tempdata$Title , tempdata$Body, sep ="\t")
     data1 <- as.data.frame(data2)

     #Normalised reviews
     data2 <- tm::removePunctuation(data2)
     data2 <- tolower(data2)
     data <- cbind.data.frame(year, rating, data1,data2)

     #Final dataset
     colnames(data) <- c("Year","Ratings","Reviews","Normalized_Reviews")
     data
     return(data)
  }
  ########################################
 
  ############Training Data###############
  output$trainData <- renderDataTable({
   # if (input$Year == "2016"){
   #  url <-  'https://www.cars.com/research/toyota-camry-2016/consumer-reviews/?pg=1&nr=350'
   # }
   # else if (input$Year == "2015"){
   #   url <-  'https://www.cars.com/research/toyota-camry-2015/consumer-reviews/?pg=1&nr=350'
   # }
   # else if (input$Year == "2014"){
   #   url <-  'https://www.cars.com/research/toyota-camry-2014/consumer-reviews/?pg=1&nr=350'
   # }
   # else if (input$Year == "2013"){
   #   url <-  'https://www.cars.com/research/toyota-camry-2013/consumer-reviews/?pg=1&nr=350'
   # }
   # else if (input$Year == "2012"){
   #   url <-  'https://www.cars.com/research/toyota-camry-2012/consumer-reviews/?pg=1&nr=350'
   # }
   # 
    url12 <- 'https://www.cars.com/research/toyota-camry-2012/consumer-reviews/?pg=1&nr=350'
    url13 <- 'https://www.cars.com/research/toyota-camry-2013/consumer-reviews/?pg=1&nr=350'
    url14 <- 'https://www.cars.com/research/toyota-camry-2014/consumer-reviews/?pg=1&nr=350'
    url15 <- 'https://www.cars.com/research/toyota-camry-2015/consumer-reviews/?pg=1&nr=350'
    url16 <- 'https://www.cars.com/research/toyota-camry-2016/consumer-reviews/?pg=1&nr=350'
    
    data12 <- webSrapping(url12,"2012")
    data13 <- webSrapping(url13,"2013")
    data14 <- webSrapping(url14,"2014")
    data15 <- webSrapping(url15,"2015")
    data16 <- webSrapping(url16,"2016")
    
    data <- rbind(data12,data13,data14,data15,data16)
    data <- as.data.frame(data)
    
    #data$Normalized_Reviews
    data2 <- subset(data, select = c("Normalized_Reviews"))
    #data2
    
    #Afinn Score Computation
    for (i in 1:nrow(data)){
      text <- as.vector(data2[i,1])
      text_df <- as.data.frame(text, stringsAsFactors = FALSE)
      text_df <- text_df %>% tidytext::unnest_tokens(word, text)
      data("stop_words")
      text_df <- text_df %>% anti_join(stop_words)
      text_df <- text_df %>% count(word)
      countn <- sum(text_df[,2])
      data$affin_score[i] <- round(((inner_join(text_df, get_sentiments("afinn")) %>% summarise(n=n()))/countn),2)
    }

    colnames(data) <- c("Year","Ratings","Reviews","Normalized Reviews","Sentiment Score")
    data
   })
  ####################################
  
  ############Test Data###############
  output$testData <- renderDataTable({
    url <- 'https://www.cars.com/research/toyota-camry-2017/consumer-reviews/?pg=1&nr=350'
    data <- webSrapping(url,"2017")

    #data$Normalized_Reviews
    #data2 <- as.data.frame(subset(data, select = c("Normalized_Reviews")))
    data2 <- data$Normalized_Reviews
    
    # print(class(data2))
    
    serviceCount <- if_else ((stringr::str_count(data2,"service") > 0 ),"Service","")
    priceCount <- if_else ((stringr::str_count(data2,"price") > 0),"Price","")
    handlingCount <- if_else ((stringr::str_count(data2,"handling") > 0 ),"Handling","")
    interiorCount <- if_else ((stringr::str_count(data2,"interior") > 0 ),"Interior","")
    tagCount <-paste(serviceCount,priceCount, handlingCount,interiorCount)
     
    data <- cbind(data, tagCount)

    colnames(data) <- c("Year","Ratings","Reviews","Normalized Reviews","Tag Reviews")
    data

  })
  #####################################
  
  ############# Sentiment Score Comparison ############
  
  
  #####################################################
  
})
