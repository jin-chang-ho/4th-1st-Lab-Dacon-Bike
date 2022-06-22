########공통으로 사용하는 함수들 모음########
library(caret)
library(lubridate)
library(e1071)
library(cvTools)
library(randomForest)
library(class)
library(tidyverse)
library(MASS)
library(xgboost)

####Min-Max Scaling(정규화) ####
normalize <- function(x) { 
  return((x-min(x))/(max(x)-min(x)))
}
  

####평가산식 함수####
nmae<-function(answer_df, submission_df){
  score <- sum(abs(answer_df-submission_df)/answer_df)/length(answer_df)
  return(score)
}


kfold <- function(classifier, ds, cl, fold) {
  acc <- c() #fold별acc 저\
  
  for (i in 1:5) {
    ts.idx<-fold$subset[which(fold$which==i)]
    ds.train <- ds[-ts.idx, ]
    ds.test <-  ds[ts.idx, ]
    cl.train <- cl[-ts.idx]
    cl.test <- cl[ts.idx]
    
    if (classifier == 'svm') {
      model <- svm(ds.train, cl.train, type='eps-regression', kernel='radial')
      svm_pred[i] <- predict(model, ds.test)
      #acc[i] <- mean(pred==cl.test)
    }
    #if (classifier == 'rf') {
    #  model <- randomForest(ds.train, cl.train, mtry = mtr,ntree = ntr,importance = TRUE) 
    #  pred <- round(predict(model, ds.test),0)
    #  acc[i] <- nmae(cl.test,pred)
    #}
    if (classifier == 'xgb') {
      acc[i] <- XgBoost(ds.train, ds.test, cl.train, cl.test)
    }
    
    
  }
  if(classifier == 'svm'){
    return(mean(svm_pred)) #예측값의 평균치를 반환
  }
  return(mean(acc))
  
}


####xgboost 모델####
XgBoost <- function(ds.tr, ds.ts, cl.tr, cl.ts) {
  #set.seed(100)
  x_train <- ds.tr %>%
    as.matrix()
  
  y_train <- cl.tr
  
  x_test <- ds.ts %>%
    as.matrix()
  
  y_test <- cl.ts
  
  dtrain <- xgb.DMatrix(x_train, label=y_train)
  
  model <- xgb.train(
    data=dtrain,
    max_depth=10,
    nround=150,
    eta=0.15,
    subsample=0.6,
    colsample_bytree=0.6,
    min_child_weight=1
  )
  
  xgb.importance(feature_names=colnames(x_train), model) %>%
    xgb.plot.importance()
  
  pred <- predict(model, x_test)
  pred <- round(pred)
  
  return(nmae(y_test, pred))
}


#### "date" 쪼개기 ####
splitData <- function(data){
  idx <- ncol(data)
  data$year <- year(data$date)
  data$month <- month(data$date)
  data$wday <- wday(data$date)
  data[,(idx+1):(idx+2)] <- lapply(data[,(idx+1):(idx+2)], as.numeric)
  data <- data[,-c(1)]
  return(data)
  
}



#### 계절데이터 범주형으로 만들기 ####
season_label<-function(data){
  idx <- ncol(data)
  data <- data[,-c(idx-3,idx-2,idx-1,idx)]
  
  for(i in range(c(1:nrow(data)))){
    if(data[i,"season.Spring"]){
      data$season <- 0
    }else if(data[i,"season.Summer"]){
      data$season <- 1
    }else if(data[i,"season.Fall"]){
      data$season <- 2
    }else if(data[i,"season.Winter"]){
      data$season <- 3
    }
  }
  return(data)
   
  
}

