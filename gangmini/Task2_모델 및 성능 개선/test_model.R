
#####테스트 데이터셋 로드######
bike_test<-read.csv("C:/Users/user/바탕 화면/Dacon_Seoul_Bike/HJK/data/test_filled.csv")
head(bike_test)
bike_test <- bike_test[,-c(1,2)]


######미세먼지 농도 범주형 feature 만들기#######
PM10_label <- c()
PM2.5_label <- c()

for(i in c(1:nrow(bike_test))){
  if(is.na(bike_test[i,"PM10"])){
    PM10_label[i] <- NA
  }else if(0 <= bike_test[i,"PM10"] && bike_test[i,"PM10"] <= 30){
    PM10_label[i] <- 1
  }else if(31 <= bike_test[i,"PM10"] && bike_test[i,"PM10"] <= 80){
    PM10_label[i] <- 2
  }else if(81 <= bike_test[i,"PM10"] && bike_test[i,"PM10"] <= 150){
    PM10_label[i] <- 3
  }else if(151 <= bike_test[i,"PM10"]){
    PM10_label[i] <- 4
  }
}

for(i in c(1:nrow(bike_test))){
  if(is.na(bike_test[i,"PM2.5"])){
    PM2.5_label[i] <- NA
  }else if(0 <= bike_test[i,"PM2.5"] && bike_test[i,"PM2.5"] <= 15){
    PM2.5_label[i] <- 1
  }else if(16 <= bike_test[i,"PM2.5"] && bike_test[i,"PM2.5"] <= 35){
    PM2.5_label[i] <- 2
  }else if(36 <= bike_test[i,"PM2.5"] && bike_test[i,"PM2.5"] <= 75){
    PM2.5_label[i] <- 3
  }else if(76 <= bike_test[i,"PM2.5"]){
    PM2.5_label[i] <- 4
  }
}

bike_test[ , "PM10_label" ] <-PM10_label
bike_test[ , "PM2.5_label" ] <-PM2.5_label


# normalization
normalize <- function(x) { # Min-Max Scaling
  return((x-min(x))/(max(x)-min(x)))
}


for(i in c(1:10)) {
  bike_test[,i]<-normalize(bike_test[,i])
}
head(bike_test)



library(cvTools)
library(randomForest)
library(class)
library(tidyverse)
library(MASS)
library(xgboost)

model <- randomForest(bike_after[,-11], bike_after$rental, mtry = 10,ntree = 300,importance = TRUE) 
pred <- round(predict(model, bike_test),0)


