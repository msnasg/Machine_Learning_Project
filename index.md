---
title       : Johns Hopkins University Practical Machine Learning Project
subtitle    : 
author      : Mohsen Asgari
job         : Portfolio Manager | msn.asg@gmail.com
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Phase 1 : Loading Packages and Downloading the required Data
  
library(caret)

library(AppliedPredictiveModeling)

library(kernlab)

library(randomForest)
  

trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

download.file(trainUrl, destfile = "E:/Coursera/train.csv")


testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(testUrl, destfile = "E:/Coursera/train.csv")

--- 
  
## Phase 2: Reading Data into the directories
  
traindata <- read.csv("E:/Coursera/train.csv",  na.strings=c("NA","#DIV/0!",""), header= TRUE)

testdata <- read.csv("E:/Coursera/train.csv", na.strings=c("NA","#DIV/0!",""), header= TRUE)

--- 
  
## Phase 3: Data preprocessing
  
traindata <- traindata[ , colSums(is.na(traindata)) == 0]

testdata <-  testdata[ , colSums(is.na(testdata)) == 0]

nearZeroColumns <- nearZeroVar(traindata, saveMetrics = TRUE)

traindata <- traindata[, nearZeroColumns$nzv==FALSE]

traindata$X <- NULL            

traindata$user_name <- NULL

traindata$cvtd_timestamp <- NULL

traindata$row.names <- NULL

traindata$new_window <- NULL

traindata$raw_timestamp_part_1 <- NULL

traindata$raw_timestamp_part_2 <- NULL

traindata$num_window <- NULL

testdata$X <- NULL

testdata$user_name <- NULL

testdata$problem_id <- NULL

testdata$cvtd_timestamp <- NULL

testdata$row.names <- NULL

testdata$new_window <- NULL

--- 
  
## Phase 4: Spliting the train data set into two parts (Training = 60% and Test = 40%)
  
trainIndex <- createDataPartition(y = traindata$classe, p=0.6,list=FALSE)

trainPartition <- traindata[trainIndex,]

testPartition <- traindata[-trainIndex,]

--- 

## Phase 5: Creating models with different 'Alghorithm'
  
set.seed(2434)

model_nb <- train(classe ~ .,  method="nb", data=trainPartition)

model_gbm <- train(classe ~ ., method = "gbm", data = trainPartition)

model_f <- train(classe ~ .,  method ="rf", data=trainPartition)

--- 
  
## Phase 6: Accuracy of the models
  
print ("Naive Bayes")

nb_accuracy <- predict(model_nb, testPartition)

print(confusionMatrix(nb_accuracy, testPartition$classe)) ### ---> The accuracy = 0.7372


print("Stochastic Gradient Boosting")

gbm_accuracy <- predict(model_gbm, testPartition)

print(confusionMatrix(gbm_accuracy, testPartition$classe)) ### ---> The accuracy = 0.958


print("Random Forest")

rf_accuracy <- predict(model_f, testPartition)

print(confusionMatrix(rf_accuracy, testPartition$classe))  ### ---> The accuracy = 0.9894

--- 
  
## Phase 7: Cross Validation
  
set.seed (1988)

fitcontrol <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

model_rf_CV <- train(classe ~ ., method="rf",  data=trainPartition, trControl = fitcontrol)

rf_CV_accuracy <- predict(model_rf_CV, testPartition)

print(confusionMatrix(rf_CV_accuracy, testPartition$classe)) ### ---> The accuracy = 0.9897

--- 
  
## Phase 8: The 20 cases predition 
  
prediction <- predict(model_rf_CV, testdata)

print(prediction)

--- 
    
## Thank You For Watching
  
   Mohsen Asgari
   
   Portfolio Manager | msn.asg@gmail.com  
