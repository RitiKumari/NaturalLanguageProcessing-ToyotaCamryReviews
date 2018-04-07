library(shiny)
library(rvest)
library(tm)
library(stringr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(caret)
library(tidyr)
library(DT)

shinyServer(function(input,output)
{
  ######### Global Variable #######
  camryData <- reactiveValues( error="ERROR", trainData="" , testData ="", trainTag = "", testTag="", trainScore ="", testScore ="", testtf="", testMatrix="")
  #camryTrain <- reactive({})
  
  
  #################################
  
  ######### Web Scrapping #######
  webSrapping <- function(url, year){
    #url <- 'https://www.cars.com/research/toyota-camry-2012/consumer-reviews/?pg=1&nr=350'
    
    web <- read_html(url)
    
    title_html <- html_nodes(web,'.cui-zeta.mmy-consumer-reviews__title span')
    title <- html_text(title_html)
    
    body_html <- html_nodes(web,'.mmy-reviews__blurb span')
    body <- html_text(body_html)
    
    rating_html <- html_nodes(web, '.cr-star-rating')
    rating <- html_attr(rating_html, "rating")
    
    
    tempdata = cbind.data.frame(title=title,body=body)
    tempdata$reviews <- paste(tempdata$title , tempdata$body, sep =" ")
    tempdata <- subset(tempdata, select = c("reviews"))
    
    #Normalised reviews
    tempdata$normalized <- tm::removePunctuation(tempdata$reviews)
    tempdata$normalized <- tolower(tempdata$normalized)
    data <- cbind.data.frame(year, rating, tempdata)
    
  }
  ########################################
  
  ########## Train Data ##################
  trainData <- function(){
    url12 <- 'https://www.cars.com/research/toyota-camry-2012/consumer-reviews/?pg=1&nr=350'
    url13 <- 'https://www.cars.com/research/toyota-camry-2013/consumer-reviews/?pg=1&nr=350'
    url14 <- 'https://www.cars.com/research/toyota-camry-2014/consumer-reviews/?pg=1&nr=350'
    url15 <- 'https://www.cars.com/research/toyota-camry-2015/consumer-reviews/?pg=1&nr=350'
    url16 <- 'https://www.cars.com/research/toyota-camry-2016/consumer-reviews/?pg=1&nr=350'
    # 
    data12 <- webSrapping(url12,"2012")
    data13 <- webSrapping(url13,"2013")
    data14 <- webSrapping(url14,"2014")
    data15 <- webSrapping(url15,"2015")
    data16 <- webSrapping(url16,"2016")
    # 
    data <- rbind.data.frame(data12,data13,data14,data15,data16)
    colnames(data) <- c("Year","Rating","Reviews","Norm_Reviews")
    camryData$trainData <- data
    print(head(camryData$trainData))
    print(class(camryData$trainData))
    data
  }
  ########################################
  
  ########## Test Data ###################
  testData <- function(){
    url <- 'https://www.cars.com/research/toyota-camry-2017/consumer-reviews/?pg=1&nr=350'
    data <- webSrapping(url,"2017")
    colnames(data) <- c("Year","Rating","Reviews","Norm_Reviews")
    camryData$testData <- data
    data
    
  }
  ########################################
  
  ########### Tag Association ############
  tagOut <- function(data){
    data2 <- data$Norm_Reviews
    serviceCount <- if_else ((stringr::str_count(data$Norm_Reviews,"service") > 0 ),"Service,","")
    priceCount <- if_else ((stringr::str_count(data$Norm_Reviews,"price") > 0),"Price,","")
    handlingCount <- if_else ((stringr::str_count(data$Norm_Reviews,"handling") > 0 ),"Handling,","")
    interiorCount <- if_else ((stringr::str_count(data$Norm_Reviews,"interior") > 0 ),"Interior","")
    tagCount <-paste(serviceCount,priceCount, handlingCount,interiorCount)
    
    data <- cbind(data, tagCount)
    colnames(data) <- c("Year","Ratings","Reviews","Norm_Reviews","Tag_Reviews")
    data
  }
  
  ########################################
  
  ############### Afinn Score #################
  afinnScore <- function(tagData){
    data = cbind(row.names(tagData),tagData)
    colnames(data)[1] <- 'id'
    
    data3 = data[1:nrow(data),]
    
    data2 <- data3 %>%  unnest_tokens(word, Norm_Reviews) %>% ungroup ()
    
    text_tidy<- data2 %>% anti_join(stop_words, by = c("word" = "word"))
    
    sentimentscore <- text_tidy %>%inner_join(get_sentiments("afinn")) %>% group_by(id)
    sentimentscore <-   aggregate(sentimentscore$score, list(sentimentscore$id), mean)
    
    colnames(sentimentscore) <- c("id" , "Score")
    sentimentscore$Score <- round(as.numeric(sentimentscore$Score),2)
    sentimentscore
    
    data <-(inner_join(data,sentimentscore))
    data
  }
  
  #############################################
  
  ########### Training Data ##############
  output$trainData <- renderDataTable({
    trainData()
    data <- isolate(camryData$trainData)
    tagData <- tagOut(data)
    camryData$testTag = tagData
    data <- afinnScore(tagData)
    camryData$trainScore <- data
    data
  })
  #######################################
  
  ############Test Data###############
  output$testData <- renderDataTable({
    testData()
    data <- isolate(camryData$testData)
    tagData <- tagOut(data)
    camryData$testTag = tagData
    data <- afinnScore(tagData)
    test <- data

    train <- isolate(camryData$trainScore)

    model <- lm(as.numeric(as.character(Ratings))~as.numeric(as.character(Score)), data = train)
    p <- predict(model, newdata = test)

    data <- cbind(data,round(p))
    colnames(data) <- c("id","Year","Ratings","Reviews","Norm_Reviews","Tag_Reviews", "Sentiment_Score","Predicted_Rating")

    camryData$testScore <- data
    
    camryData$testData
    
  })
  #####################################
  
  ########## Computed Train ###########
  
  output$computedTrain <- renderDataTable({
    camryData$trainScore 
  })
  
  ##################################### 
  
  ######### Computed Test #############
  output$computedTest <- renderDataTable({
    camryData$testScore 
    })
  #####################################
  
  # ########### Accuracy ###############
  output$confmatrix <- renderPrint({
    data <- isolate(camryData$testScore)
    class(data)
    PRating <- matrix(data$Predicted_Rating,ncol = 1)
    URating <- matrix(as.numeric(data$Ratings),ncol=1)
    acctable <- table(factor(PRating, levels=min(URating):max(URating)), factor(URating, levels=min(URating):max(URating)))
    camryData$testMatrix <- acctable
    
    acctable
  })
  ####################################
  
  ########## Accuracy ###############
  output$accuracy <- renderPrint({
    data <- isolate(camryData$testMatrix)
    testData <- isolate(camryData$testScore)

    for(i in 1:5){
      a = sum(as.numeric(data[i,i]))
    }
    accuracy = a/nrow(testData)
    accuracy * 100 

  })
  ###################################
  
  
  ########## Tagged Train ############
  output$tfidf <- renderDataTable({
    data <- isolate(camryData$trainData)
    tagData <- tagOut(data)
    camryData$trainTag = tagData
    tagData
    
    separatedData <- separate_rows(tagData, Tag_Reviews, convert = TRUE)
    
    custom_stop_words <- bind_rows(data_frame(word = c("a4","2012","2012s","2017","2001","4","2009","2002","1997",
                                                       "14k","2728","275","16","14k","179","20300","2003","40","2","nor","1984","e","8th","9th","08"), 
                                              lexicon = c("custom")), 
                                   stop_words)
    
    
    book_words <- separatedData %>%
      unnest_tokens(word, Norm_Reviews) %>%
      anti_join(custom_stop_words) %>%
      dplyr::count(word, Tag_Reviews,sort=TRUE) %>%
      ungroup() %>%
      bind_tf_idf(word, Tag_Reviews, n) %>% arrange(-tf_idf) 
    
    camryData$testtf <- book_words
    colnames(book_words)[1] <- c("Word")
    colnames(book_words)[3] <- c("Count")
    book_words
    
  })
  ####################################
  
  ############# Plot #################
  output$tfPlot <- renderPlot({
    book_words <- isolate(camryData$testtf)
    s <- head(book_words[grep("Service",book_words$Tag_Reviews),],10,book_words$tf_idf)
    p <- head(book_words[grep("Price",book_words$Tag_Reviews),],10,book_words$tf_idf)
    h <- head(book_words[grep("Handling",book_words$Tag_Reviews),],10,book_words$tf_idf)
    i <- head(book_words[grep("Interior",book_words$Tag_Reviews),],10,book_words$tf_idf)
     
    book_words <- rbind(s,p,h,i)
    book_words
    
    book_words %>%  mutate(word = factor(word, levels = rev(unique(word)))) %>%
      ggplot(aes(word, tf_idf, fill = Tag_Reviews)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~Tag_Reviews, ncol = 2, scales = "free") +
      coord_flip() +
      labs(title = "Top 10 most popular words in Camry Reviews in 2017",
           caption = "Toyota camry @ https://www.cars.com/research/toyota-camry-2017/consumer-reviews/",
           x = NULL, y = "tf-idf")
  })
  ####################################
  
  
  ############## Average Sentiment Score #############
  output$sentiment <- renderDataTable({
    data <- isolate(camryData$testScore)
    rowCount <- nrow(data)
    avgScore <- sum(as.numeric(as.character(data$Sentiment_Score))) / rowCount
    avgRating <- sum(as.numeric(as.character(data$Ratings))) / rowCount
    
    serviceData <- data[grep("Service",data$Tag_Reviews),]
    priceData <- data[grep("Price",data$Tag_Reviews),]
    handlingData <- data[grep("Handling",data$Tag_Reviews),]
    interiorData <- data[grep("Interior",data$Tag_Reviews),]
    
    avgServiceScore <- (sum(as.numeric(as.character(serviceData$Sentiment_Score))) / nrow(serviceData))
    avgPriceScore <- (sum(as.numeric(as.character(priceData$Sentiment_Score))) / nrow(priceData))
    avgHandlingScore <- (sum(as.numeric(as.character(handlingData$Sentiment_Score))) / nrow(handlingData))
    avgInteriorScore <- (sum(as.numeric(as.character(interiorData$Sentiment_Score))) / nrow(interiorData))
    
    avgRServiceScore <- (sum(as.numeric(as.character(serviceData$Ratings))) / nrow(serviceData))
    avgRPriceScore <- (sum(as.numeric(as.character(priceData$Ratings))) / nrow(priceData))
    avgRHandlingScore <- (sum(as.numeric(as.character(handlingData$Ratings))) / nrow(handlingData))
    avgRInteriorScore <- (sum(as.numeric(as.character(interiorData$Ratings))) / nrow(interiorData))
     
    scoreVec <- c(avgScore, avgRating, avgRServiceScore, avgServiceScore, avgRPriceScore, 
                  avgPriceScore ,avgRHandlingScore, avgHandlingScore,avgRInteriorScore, avgInteriorScore)
    scoreVec <- data.frame(scoreVec)
    
    scoreTitle <- c("Avg Sentiment Score ","Avg User Rating","Avg Service Rating","Avg Sentiment for Sevice Tag",
                     "Avg Price Rating","Avg Sentiment for Price Tag","Avg Handling Rating", "Avg Sentiment for Handling Tag",
                    "Avg Interior Rating","Avg Sentiment for Interior Tag")
    scoreTitle <- data.frame(scoreTitle)
    
    data <- cbind(scoreTitle,scoreVec)
    colnames(data) <- c("Entity","Calculated Rating")
    data
    
    })
  
  ####################################################
  
})
